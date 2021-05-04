import re
from datetime import datetime

import sqlalchemy
from sqlalchemy import Column, ForeignKey, Integer, Table
from sqlalchemy.orm import relationship
from sqlalchemy.orm.exc import NoResultFound
from zeeguu_core.constants import JSON_TIME_FORMAT
from zeeguu_core.definition_of_learned import is_learned_based_on_exercise_outcomes
from zeeguu_core.model import Article

from wordstats import Word

import zeeguu_core
from zeeguu_core.model.SortedExerciseLog import SortedExerciseLog
from zeeguu_core.model.exercise import Exercise
from zeeguu_core.model.exercise_outcome import ExerciseOutcome
from zeeguu_core.model.exercise_source import ExerciseSource
from zeeguu_core.model.language import Language
from zeeguu_core.model.text import Text
from zeeguu_core.model.url import Url
from zeeguu_core.model.user import User
from zeeguu_core.model.user_word import UserWord
from zeeguu_core.bookmark_quality.fit_for_study import fit_for_study

db = zeeguu_core.db

CORRECTS_IN_A_ROW_FOR_LEARNED = 4

bookmark_exercise_mapping = Table('bookmark_exercise_mapping',
                                  db.Model.metadata,
                                  Column('bookmark_id', Integer,
                                         ForeignKey('bookmark.id')),
                                  Column('exercise_id', Integer,
                                         ForeignKey('exercise.id'))
                                  )

WordAlias = db.aliased(UserWord, name="translated_word")


class Bookmark(db.Model):
    __table_args__ = {'mysql_collate': 'utf8_bin'}

    id = db.Column(db.Integer, primary_key=True)

    origin_id = db.Column(db.Integer, db.ForeignKey(UserWord.id), nullable=False)
    origin = db.relationship(UserWord, primaryjoin=origin_id == UserWord.id)

    translation_id = db.Column(db.Integer, db.ForeignKey(UserWord.id), nullable=False)
    translation = db.relationship(UserWord, primaryjoin=translation_id == UserWord.id)

    user_id = db.Column(db.Integer, db.ForeignKey(User.id))
    user = db.relationship(User)

    text_id = db.Column(db.Integer, db.ForeignKey(Text.id))
    text = db.relationship(Text)

    time = db.Column(db.DateTime)

    exercise_log = relationship(Exercise,
                                secondary="bookmark_exercise_mapping",
                                order_by="Exercise.id")

    starred = db.Column(db.Boolean, default=False)

    learned = db.Column(db.Boolean, default=False)

    fit_for_study = db.Column(db.Boolean)

    learned_time = db.Column(db.DateTime)

    def __init__(self, origin: UserWord, translation: UserWord, user: 'User',
                 text: str, time: datetime):
        self.origin = origin
        self.translation = translation
        self.user = user
        self.time = time
        self.text = text
        self.stared = False
        self.fit_for_study = fit_for_study(self)

    def __repr__(self):
        return "Bookmark[{3} of {4}: {0}->{1} in '{2}...']\n". \
            format(self.origin.word, self.translation.word,
                   self.text.content[0:10], self.id, self.user_id)

    def serializable_dictionary(self):
        return dict(
            origin=self.origin.word,
            translation=self.translation.word,
            context=self.text.content
        )

    def add_new_exercise(self, exercise):
        self.exercise_log.append(exercise)

    def translations_rendered_as_text(self):
        return self.translation.word

    def content_is_not_too_long(self):
        return len(self.text.content) < 60

    def update_fit_for_study(self, session=None):
        """
            Called when something happened to the bookmark,
             that requires it's "fit for study" status to be
              updated. Including:
              - starred / unstarred
              - exercise finished for the given bookmark
              - ...

        :param session:
        :return:
        """
        self.fit_for_study = fit_for_study(self)
        if session:
            session.add(self)

    def add_new_exercise_result(self,
                                exercise_source: ExerciseSource,
                                exercise_outcome: ExerciseOutcome,
                                exercise_solving_speed):

        exercise = Exercise(exercise_outcome, exercise_source, exercise_solving_speed,
                            datetime.now())

        self.add_new_exercise(exercise)
        db.session.add(exercise)

        return exercise

    def report_exercise_outcome(self,
                                exercise_source: str,
                                exercise_outcome: str,
                                exercise_solving_speed,
                                db_session):

        from zeeguu_core.model import UserExerciseSession
        from zeeguu_core.word_scheduling.arts.bookmark_priority_updater import BookmarkPriorityUpdater

        new_source = ExerciseSource.find_or_create(db_session, exercise_source)
        new_outcome = ExerciseOutcome.find_or_create(db_session, exercise_outcome)

        exercise = self.add_new_exercise_result(new_source,
                                                new_outcome,
                                                exercise_solving_speed)
        db_session.add(exercise)
        db_session.commit()

        self.update_fit_for_study(db_session)
        self.update_learned_status(db_session)

        UserExerciseSession.update_exercise_session(exercise, db_session)
        BookmarkPriorityUpdater.update_bookmark_priority(db, self.user)

    def split_words_from_context(self):

        result = []
        bookmark_content_words = re.findall(r'(?u)\w+', self.text.content)
        for word in bookmark_content_words:
            if word.lower() != self.origin.word.lower():
                result.append(word)

        return result

    def context_word_count(self):
        words = self.split_words_from_context()
        return len(words)

    def json_serializable_dict(self, with_context=True, with_title=False):
        try:
            translation_word = self.translation.word
        except AttributeError as e:
            translation_word = ''
            zeeguu_core.log(f"Exception caught: for some reason there was no translation for {self.id}")
            print(str(e))

        word_info = Word.stats(self.origin.word,
                               self.origin.language.code)

        learned_datetime = str(self.learned_time.date()) if self.learned else ''

        created_day = "today" if self.time.date() == datetime.now().date() else ''

        bookmark_title = ""
        if with_title:
            try:
                bookmark_title = self.text.article.title
            except Exception as e:
                from sentry_sdk import capture_exception
                capture_exception(e)
                print(f"could not find article title for bookmark with id: {self.id}")

        result = dict(
            id=self.id,
            to=translation_word,
            from_lang=self.origin.language.code,
            to_lang=self.translation.language.code,
            title=bookmark_title,
            url=self.text.url.as_string(),
            origin_importance=word_info.importance,
            learned_datetime=SortedExerciseLog(self).str_most_recent_correct_dates(),
            origin_rank=word_info.rank if word_info.rank != 100000 else '',
            starred=self.starred if self.starred is not None else False,
            article_id=self.text.article_id if self.text.article_id else '',
            created_day=created_day,  # human readable stuff...
            time=self.time.strftime(JSON_TIME_FORMAT)
        )

        if self.text.article:
            result['article_title'] = self.text.article.title

        result["from"] = self.origin.word
        if with_context:
            result['context'] = self.text.content
        return result

    @classmethod
    def find_or_create(cls, session, user,
                       _origin: str, _origin_lang: str,
                       _translation: str, _translation_lang: str,
                       _context: str, _url: str, _url_title: str, article_id: int):
        """
            if the bookmark does not exist, it creates it and returns it
            if it exists, it ** updates the translation** and returns the bookmark object

        :param _origin:
        :param _context:
        :param _url:
        :return:
        """

        origin_lang = Language.find_or_create(_origin_lang)
        translation_lang = Language.find_or_create(_translation_lang)

        origin = UserWord.find_or_create(session, _origin, origin_lang)

        article = Article.query.filter_by(id=article_id).one()

        url = Url.find_or_create(session, article.url.as_string(), _url_title)

        context = Text.find_or_create(session, _context, origin_lang, url, article)

        translation = UserWord.find_or_create(session, _translation, translation_lang)

        now = datetime.now()

        try:
            # try to find this bookmark
            bookmark = Bookmark.find_by_user_word_and_text(user, origin,
                                                           context)

            # update the translation
            bookmark.translation = translation

        except sqlalchemy.orm.exc.NoResultFound as e:
            bookmark = cls(origin, translation, user, context, now)
        except Exception as e:
            raise e

        session.add(bookmark)
        session.commit()

        return bookmark

    @classmethod
    def find_by_specific_user(cls, user):
        return cls.query.filter_by(
            user=user
        ).all()

    @classmethod
    def find_all(cls):
        return cls.query.filter().all()

    @classmethod
    def find_all_for_text_and_user(cls, text, user):
        return Bookmark.query.filter_by(text=text, user=user).all()

    @classmethod
    def find_all_for_user_and_url(cls, user, url):
        return cls.query.join(Text).filter(Text.url == url).filter(Bookmark.user == user).all()

    @classmethod
    def find(cls, b_id):
        return cls.query.filter_by(
            id=b_id
        ).one()

    @classmethod
    def find_all_by_user_and_word(cls, user, word):
        return cls.query.filter_by(
            user=user,
            origin=word
        ).all()

    @classmethod
    def find_by_user_word_and_text(cls, user, word, text):
        return cls.query.filter_by(
            user=user,
            origin=word,
            text=text
        ).one()

    @classmethod
    def exists(cls, bookmark):
        try:
            cls.query.filter_by(
                origin_id=bookmark.origin.id,
                id=bookmark.id
            ).one()
            return True
        except NoResultFound:
            return False

    def update_learned_status(self, session):
        """
            To call when something happened to the bookmark,
             that requires it's "learned" status to be updated.
        :param session:
        :return:
        """

        log = SortedExerciseLog(self)
        is_learned = is_learned_based_on_exercise_outcomes(log)
        if is_learned:
            zeeguu_core.log(f"Log: {log.summary()}: bookmark {self.id} learned!")
            self.learned_time = log.last_exercise_time()
            self.learned = True
            session.add(self)
        else:
            zeeguu_core.log(f"Log: {log.summary()}: bookmark {self.id} not learned yet.")
