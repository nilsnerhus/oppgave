# Okokok, nå har jeg laga funksjonene i egne script, sånn at jeg enklere forstår
# hele prosessen

processed_text <- preprocess(nap_data)

optimal_result <- optimal_topics(processed_text)

topic_model(optimal_result)