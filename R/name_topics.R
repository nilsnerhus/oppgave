# Convert simple character vector to proper topics structure
name_topics <- function(
	model,
	topic_names_vector,
	meaningful_threshold = 0.07
) {
	log_message("Converting topic names to proper structure", "name_topics")

	# Extract model components
	stm_model <- model$data$model
	theta <- model$data$topic_proportions
	meta <- model$data$aligned_meta
	k <- stm_model$settings$dim$K

	# Validate input
	if (length(topic_names_vector) != k) {
		stop(paste(
			"topic_names_vector must contain exactly",
			k,
			"names for",
			k,
			"topics"
		))
	}

	# Calculate topic proportions and document spread
	topic_proportions <- colMeans(theta)

	# Calculate effective document count (meaningful presence threshold)
	effective_doc_counts <- numeric(k)

	for (i in 1:k) {
		effective_doc_counts[i] <- sum(
			theta[, i] > meaningful_threshold
		)
	}

	# Get FREX terms from model (stored by fit_model)
	if ("frex_terms" %in% names(model$data)) {
		frex_terms_strings <- sapply(
			model$data$frex_terms,
			function(x) paste(x[1:5], collapse = ", ")
		)
	} else {
		# Fallback: generate FREX terms
		topic_labels <- stm::labelTopics(stm_model, n = 5)
		frex_terms_strings <- apply(topic_labels$frex, 1, function(x) {
			paste(x[1:5], collapse = ", ")
		})
	}

	# Calculate top countries for each topic (store as JSON strings for RDS compatibility)
	if ("country_name" %in% names(meta)) {
		top_countries_list <- list()

		for (i in 1:k) {
			country_scores <- aggregate(
				theta[, i],
				by = list(country = meta$country_name),
				FUN = mean
			)
			top_n <- country_scores[
				order(country_scores$x, decreasing = TRUE),
			]

			# Store as list
			top_countries_list[[i]] <- data.frame(
				country = top_n$country,
				score = top_n$x,
				stringsAsFactors = FALSE
			)
		}

		# Convert to JSON strings for storage (survives RDS save/load)
		top_countries_json <- sapply(
			top_countries_list,
			jsonlite::toJSON
		)
	} else {
		# Empty JSON arrays
		top_countries_json <- rep('{"country":[],"score":[]}', k)
	}

	# Create the data frame
	topics_table <- data.frame(
		topic_id = 1:k,
		topic_name = topic_names_vector,
		frex_terms = frex_terms_strings,
		top_countries = top_countries_json, # Stored as JSON strings
		topic_proportion = topic_proportions,
		effective_documents = effective_doc_counts,
		stringsAsFactors = FALSE
	)

	# Create the data frame that calculate_metrics expects
	topics_table <- data.frame(
		topic_id = 1:k,
		topic_name = topic_names_vector,
		frex_terms = frex_terms_strings,
		top_countries = top_countries_json,
		topic_proportion = topic_proportions,
		effective_documents = effective_doc_counts,
		stringsAsFactors = FALSE
	)

	log_message(
		paste("Created topics structure with", k, "topics"),
		"name_topics"
	)

	# Return in the format that calculate_metrics expects
	return(create_result(
		data = topics_table,
		metadata = list(
			timestamp = Sys.time(),
			k = k,
			manual_names_used = TRUE,
			meaningful_threshold = meaningful_threshold, # ADD THIS
			success = TRUE
		),
		diagnostics = list()
	))
}

