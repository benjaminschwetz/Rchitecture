import zeeguu_core
from zeeguu_core.model import Article, Language, LocalizedTopic

session = zeeguu_core.db.session

counter = 0

languages = Language.available_languages()
languages = [Language.find('da')]

for language in languages:
    articles = Article.query.filter(Article.language == language).order_by(Article.id.desc()).all()

    loc_topics = LocalizedTopic.all_for_language(language)

    total_articles = len(articles)
    for article in articles:
        counter += 1
        print(f"{article.title}")
        print(f"{article.url.as_string()}")
        for loc_topic in loc_topics:
            if loc_topic.matches_article(article):
                article.add_topic(loc_topic.topic)
                print(f" #{loc_topic.topic_translated}")
        print("")
        session.add(article)

        if counter % 1000 == 0:
            percentage = (100 * counter / total_articles) / 100
            print(f"{counter} dorticles done ({percentage}%). last article id: {article.id}. Comitting... ")
            session.commit()

percentage = (100 * counter / total_articles) / 100
print(f"{counter} dorticles done ({percentage}%). last article id: {article.id}. Comitting... ")
session.commit()
