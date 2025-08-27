# Test OpenAI directly
test_openai <- function() {
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  cat("API Key present:", nchar(openai_key) > 0, "\n")
  
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      "Authorization" = paste("Bearer", openai_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(list(
      model = "gpt-3.5-turbo",
      messages = list(list(role = "user", content = "Generate exactly 2 words as a topic label for: cyclon, tropic, decad")),
      max_tokens = 10,
      temperature = 0.1
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  
  cat("Status:", httr::status_code(response), "\n")
  cat("Response:", httr::content(response, "text"), "\n")
}

test_openai()