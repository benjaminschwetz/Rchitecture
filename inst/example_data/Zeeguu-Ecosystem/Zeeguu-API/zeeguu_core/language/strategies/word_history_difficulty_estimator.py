from numpy import mean
from wordstats import Word, WordInfo
from zeeguu_core import model
from zeeguu_core.language.difficulty_estimator_strategy import DifficultyEstimatorStrategy
from zeeguu_core.util.text import split_words_from_text
from zeeguu_core.model.word_knowledge.word_interaction_history import WordInteractionHistory
from zeeguu_core.model import UserWord, Language
from nltk.stem import SnowballStemmer
from collections import defaultdict, Counter

from zeeguu_core.constants import WIH_READ_CLICKED, WIH_READ_NOT_CLICKED_IN_SENTENCE, WIH_READ_NOT_CLICKED_OUT_SENTENCE, WIH_WRONG_EX_RECOGNIZE,\
WIH_WRONG_EX_TRANSLATE,WIH_WRONG_EX_CHOICE,WIH_WRONG_EX_MATCH

class WordHistoryDifficultyEstimator(DifficultyEstimatorStrategy):

    CUSTOM_NAMES = ["history"]

    def __init__(self, language: 'model.Language', user: 'model.User'):
        self.user = user
        self.language = language
        self.score_map = dict()

        # determines word scores
        #words_history = WordInteractionHistory.find_all_word_histories_for_user_language(user, language)

        #words_found = [w.word.word.lower() for w in words_history]
        #event_history = [w.interaction_history for w in words_history]

        #words_score = [max(1 - len(e) / 1, 0) for e in event_history]

    # creates estimator that determines the ratio of new words for a given text
    @classmethod
    def recurrence(cls, language: 'model.Language', user: 'model.User'):
        """
                This estimator computes the ratio of new words for a given user and language
                :param language: language of the text that needs to be estimated
                :param user: the user for which the difficulty estimation needs to be done
                :rtype: WordHistoryDifficultyEstimator
                :return: WordHistoryDifficultyEstimator with initialized user, language and word => score map
                        which can be used for determining scores for multiple articles for the same user and language
        """

        estimator = cls(language, user)

        # determine word scores
        words_history = WordInteractionHistory.find_all_word_histories_for_user_language(user, language)
        words_found = [w.word.word for w in words_history]
        event_history = [w.interaction_history for w in words_history]



        words_score = [0 for e in event_history]

        estimator.score_map = dict(zip(words_found, words_score))

        return estimator

    @classmethod
    def recurrence_until_timestamp(cls, language: 'model.Language', user: 'model.User',max_timestamp):
        """
                This estimator computes the ratio of new words for a given user and language
                :param language: language of the text that needs to be estimated
                :param user: the user for which the difficulty estimation needs to be done
                :rtype: WordHistoryDifficultyEstimator
                :return: WordHistoryDifficultyEstimator with initialized user, language and word => score map
                        which can be used for determining scores for multiple articles for the same user and language
        """

        estimator = cls(language, user)

        # determine word scores
        words_history = WordInteractionHistory.find_all_word_histories_for_user_language(user, language)

        words_found = []
        event_history = []
        for wh in words_history:
            history = [event for event in wh.interaction_history if event.seconds_since_epoch <= max_timestamp]

            if len(history) > 0:
                words_found.append(wh.word.word)
                event_history.append(history)

        words_score = [0 for e in event_history]

        estimator.score_map = dict(zip(words_found, words_score))

        return estimator

    @classmethod
    def difficulty(cls, language: 'model.Language', user: 'model.User', mode = 1):
        """
                This estimator computes the ratio of new words for a given user and language
                :param language: language of the text that needs to be estimated
                :param user: the user for which the difficulty estimation needs to be done
                :rtype: WordHistoryDifficultyEstimator
                :return: WordHistoryDifficultyEstimator with initialized user, language and word => score map
                        which can be used for determining scores for multiple articles for the same user and language
        """

        estimator = cls(language, user)

        # determine word scores
        words_history = WordInteractionHistory.find_all_word_histories_for_user_language(user, language)

        words_found = [w.word.word for w in words_history]
        event_history = [w.interaction_history for w in words_history]

        words_score = []

        if mode == 1:

            for e in event_history:
                if len(e) > 10:
                    words_score.append(0)
                else:
                    N_seen_context = sum([event == WIH_READ_NOT_CLICKED_IN_SENTENCE for event in event_history])
                    N_seen = sum([event == WIH_READ_NOT_CLICKED_OUT_SENTENCE for event in event_history])
                    # N_clicked = sum([event == WIH_READ_CLICKED for event in event_history])

                    if N_seen_context > 3 or N_seen > 6:
                        words_score.append(0)
                    else:
                        words_score.append(min(1 - N_seen_context / 4, 1 - N_seen / 7))
        elif mode == 2:
            for e in event_history:
                words_score.append(max(1 - len(e)/10,0))

        elif mode == 3:
            length = 0
            for e in event_history[::-1]:
                if e is not WIH_READ_CLICKED or e is not WIH_WRONG_EX_CHOICE or e is not WIH_WRONG_EX_MATCH or e is not WIH_WRONG_EX_RECOGNIZE or\
                    e is not WIH_WRONG_EX_TRANSLATE:
                    length += 1
                words_score.append(max(1 - len(e)/10,0))

        estimator.score_map = dict(zip(words_found, words_score))

        return estimator

    @classmethod
    def difficulty_until_timestamp(cls, language: 'model.Language', user: 'model.User', max_timestamp, mode = 1, scaling = 10.0, scaling2 = 20.0):
        """
                This estimator computes the ratio of new words for a given user and language
                :param language: language of the text that needs to be estimated
                :param user: the user for which the difficulty estimation needs to be done
                :rtype: WordHistoryDifficultyEstimator
                :return: WordHistoryDifficultyEstimator with initialized user, language and word => score map
                        which can be used for determining scores for multiple articles for the same user and language
        """

        estimator = cls(language, user)

        # determine word scores
        words_history = WordInteractionHistory.find_all_word_histories_for_user_language(user, language)

        words_found = []
        event_history = []
        for wh in words_history:
            history = [event for event in wh.interaction_history if event.seconds_since_epoch <= max_timestamp]

            if history:
                words_found.append(wh.word.word)
                event_history.append(history)

        words_score = []

        if mode == 1:

            for e in event_history:
                N_seen_context = sum([event.event_type == WIH_READ_NOT_CLICKED_IN_SENTENCE for event in e])
                N_seen = sum([event.event_type == WIH_READ_NOT_CLICKED_OUT_SENTENCE for event in e])
                # N_clicked = sum([event == WIH_READ_CLICKED for event in event_history])

                words_score.append(max(0,1 - N_seen_context / scaling - N_seen / scaling2))
        elif mode == 2:
            for e in event_history:
                words_score.append(max(1 - len(e)/scaling,0))

        elif mode == 3:
            length = 0
            for e in event_history:
                events= e[::-1]
                for event in events:
                    if event.event_type is not WIH_READ_CLICKED or event.event_type is not WIH_WRONG_EX_CHOICE or event.event_type is not WIH_WRONG_EX_MATCH or event.event_type is not WIH_WRONG_EX_RECOGNIZE or \
                            event.event_type is not WIH_WRONG_EX_TRANSLATE:
                        length += 1
                words_score.append(max(1 - len(e)/scaling,0))


        estimator.score_map = dict(zip(words_found, words_score))

        return estimator


    def estimate_difficulty(self, text: str):
        """
        This estimator computes the difficulty based on the scoring map
        :param text: See DifficultyEstimatorStrategy
        :rtype: dict
        :return: The dictionary contains the keys and return types
                    normalized: float (0<=normalized<=1)
                    discrete: string [EASY, MEDIUM, HARD]
        """

        # Calculate difficulty for each word
        words = split_words_from_text(text)
        stemmer = SnowballStemmer(self.language.name.lower())
        words = [stemmer.stem(w.lower()) for w in words]

        # frequency and length
        word_frequency = Counter(words)
        total_words = len(words)


        # score per word
        word_scores = {w: self.word_difficulty(self.score_map, True, w) for w in word_frequency}


        # If we can't compute the text difficulty, we estimate hard
        if (len(word_scores)) == 0:
            difficulty_scores = dict(
                median=1.0,
                median_unique=1.0,
                normalized=1.0,
                discrete= "HARD",
                unique_ratio=1.0
            )
        else:

            # words above median score
            word_scores_sorted= sorted(word_scores.items(), key=lambda item: item[1])
            center = int(round(len(word_scores_sorted) / 2, 0))
            word_scores_median = dict(word_scores_sorted[center:])
            total_words_median = sum([word_frequency[w] for w in word_scores_median])

            # If we can't compute the text difficulty, we estimate hard

            difficulty_scores = dict(
                median=sum([ s*word_frequency[w] for w, s in word_scores_median.items()])/total_words_median,
                median_unique=mean(list(word_scores_median.values())),
                normalized=sum([ s*word_frequency[w] for w, s in word_scores.items()])/total_words,  # Originally called 'score_average'
                #todo: discrete=discrete_difficulty,               # Originally called 'estimated_difficulty'
                unique_ratio=mean(list(word_scores.values()))      # Ratio of unknown
            )

        return difficulty_scores

    @classmethod
    def discrete_text_difficulty(cls, median_difficulty: float):
        """
        :param median_difficulty:
        :return: a symbolic representation of the estimated difficulty
         the values are between "EASY", "MEDIUM", and "HARD"
        """
        if median_difficulty < 0.3:
            return "EASY"
        if median_difficulty < 0.4:
            return "MEDIUM"
        return "HARD"

    @classmethod
    # TODO: must test this thing
    def word_difficulty(cls, known_probabilities: dict, personalized: bool, word: Word):
        """
        # estimate the difficulty of a word, given:
            :param word_info:
            :param known_probabilities:
            :param personalized:
            :param word:

        :return: a normalized value where 0 is (easy) and 1 is (hard)
        """

        # Assume word is difficult and unknown
        estimated_difficulty = 1.0

        # Check if the user knows the word
        try:
            known_probability = known_probabilities[word]  # Value between 0 (unknown) and 1 (known)
        except KeyError:
            known_probability = None

        if personalized and known_probability is not None:
            estimated_difficulty = float(known_probability)

        return estimated_difficulty

