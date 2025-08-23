#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

read_benchmark_data <- function(csv_file = "benchmark_results.csv") {
  if (!file.exists(csv_file)) {
    stop(paste("Benchmark CSV file not found:", csv_file))
  }
  
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Ensure we have all required columns
  required_cols <- c("test_name", "data_size", "msgpack_encode_ms", "msgpack_decode_ms", 
                    "msgpack_size_bytes", "json_encode_ms", "json_decode_ms", "json_size_bytes")
  
  if (!all(required_cols %in% colnames(data))) {
    stop("CSV file missing required columns")
  }
  
  return(data)
}

# Create encoding performance comparison plot
plot_encoding_performance <- function(data) {
  # Reshape data for plotting
  encode_data <- data %>%
    select(test_name, data_size, msgpack_encode_ms, json_encode_ms) %>%
    pivot_longer(cols = c(msgpack_encode_ms, json_encode_ms),
                names_to = "format", values_to = "encode_time") %>%
    mutate(format = case_when(
      format == "msgpack_encode_ms" ~ "MessagePack",
      format == "json_encode_ms" ~ "JSON"
    ))
  
  p <- ggplot(encode_data, aes(x = data_size, y = encode_time, color = format, shape = format)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_x_log10(labels = scales::comma_format()) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(
      title = "Encoding Performance: MessagePack vs JSON",
      subtitle = "Lower is better",
      x = "Data Size (number of items)",
      y = "Encoding Time (ms)",
      color = "Format",
      shape = "Format"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("MessagePack" = "#2E86AB", "JSON" = "#A23B72"))
  
  return(p)
}

# Create decoding performance comparison plot
plot_decoding_performance <- function(data) {
  decode_data <- data %>%
    select(test_name, data_size, msgpack_decode_ms, json_decode_ms) %>%
    pivot_longer(cols = c(msgpack_decode_ms, json_decode_ms),
                names_to = "format", values_to = "decode_time") %>%
    mutate(format = case_when(
      format == "msgpack_decode_ms" ~ "MessagePack",
      format == "json_decode_ms" ~ "JSON"
    ))
  
  p <- ggplot(decode_data, aes(x = data_size, y = decode_time, color = format, shape = format)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_x_log10(labels = scales::comma_format()) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(
      title = "Decoding Performance: MessagePack vs JSON",
      subtitle = "Lower is better",
      x = "Data Size (number of items)",
      y = "Decoding Time (ms)",
      color = "Format",
      shape = "Format"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("MessagePack" = "#2E86AB", "JSON" = "#A23B72"))
  
  return(p)
}

# Create size comparison plot
plot_size_comparison <- function(data) {
  size_data <- data %>%
    select(test_name, data_size, msgpack_size_bytes, json_size_bytes) %>%
    pivot_longer(cols = c(msgpack_size_bytes, json_size_bytes),
                names_to = "format", values_to = "size_bytes") %>%
    mutate(format = case_when(
      format == "msgpack_size_bytes" ~ "MessagePack",
      format == "json_size_bytes" ~ "JSON"
    ))
  
  p <- ggplot(size_data, aes(x = data_size, y = size_bytes, color = format, shape = format)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_x_log10(labels = scales::comma_format()) +
    scale_y_log10(labels = scales::comma_format(suffix = " B")) +
    labs(
      title = "Serialized Size Comparison: MessagePack vs JSON",
      subtitle = "Lower is better",
      x = "Data Size (number of items)",
      y = "Serialized Size (bytes)",
      color = "Format",
      shape = "Format"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("MessagePack" = "#2E86AB", "JSON" = "#A23B72"))
  
  return(p)
}

# Create size efficiency plot (MessagePack as percentage of JSON size)
plot_size_efficiency <- function(data) {
  efficiency_data <- data %>%
    mutate(size_ratio = (msgpack_size_bytes / json_size_bytes) * 100)
  
  p <- ggplot(efficiency_data, aes(x = data_size, y = size_ratio)) +
    geom_point(color = "#2E86AB", size = 3) +
    geom_line(color = "#2E86AB", size = 1) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray", alpha = 0.7) +
    scale_x_log10(labels = scales::comma_format()) +
    ylim(0, 100) +
    labs(
      title = "MessagePack Size Efficiency",
      subtitle = "MessagePack size as percentage of JSON size (lower is better)",
      x = "Data Size (number of items)",
      y = "MessagePack Size (% of JSON)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ) +
    annotate("text", x = max(efficiency_data$data_size) * 0.7, y = 52, 
             label = "50% threshold", color = "red", size = 3)
  
  return(p)
}

# Create performance speedup plot
plot_performance_speedup <- function(data) {
  speedup_data <- data %>%
    mutate(
      encode_speedup = json_encode_ms / msgpack_encode_ms,
      decode_speedup = json_decode_ms / msgpack_decode_ms
    ) %>%
    select(test_name, data_size, encode_speedup, decode_speedup) %>%
    pivot_longer(cols = c(encode_speedup, decode_speedup),
                names_to = "operation", values_to = "speedup") %>%
    mutate(operation = case_when(
      operation == "encode_speedup" ~ "Encoding",
      operation == "decode_speedup" ~ "Decoding"
    ))
  
  p <- ggplot(speedup_data, aes(x = data_size, y = speedup, color = operation, shape = operation)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray", alpha = 0.7) +
    scale_x_log10(labels = scales::comma_format()) +
    labs(
      title = "MessagePack Performance Speedup vs JSON",
      subtitle = "Higher is better (values > 1 mean MessagePack is faster)",
      x = "Data Size (number of items)",
      y = "Speedup Factor (times faster)",
      color = "Operation",
      shape = "Operation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("Encoding" = "#F18F01", "Decoding" = "#C73E1D")) +
    annotate("text", x = max(speedup_data$data_size) * 0.7, y = 1.05, 
             label = "Equal performance", color = "gray", size = 3)
  
  return(p)
}

main <- function() {
  cat(" Generating MessagePack vs JSON benchmark plots...\n")
  
  data <- read_benchmark_data()
  cat(paste(" Loaded", nrow(data), "benchmark results\n"))
  
  if (!dir.exists("graphs")) {
    dir.create("graphs")
  }
  
  # Generate plots
  plots <- list(
    encoding_performance = plot_encoding_performance(data),
    decoding_performance = plot_decoding_performance(data),
    size_comparison = plot_size_comparison(data),
    size_efficiency = plot_size_efficiency(data),
    performance_speedup = plot_performance_speedup(data)
  )
  
  # Save plots
  for (name in names(plots)) {
    filename <- paste0("graphs/", name, ".png")
    cat(paste(" Saving", filename, "\n"))
    
    ggsave(filename, plots[[name]], 
           width = 10, height = 6, dpi = 300, bg = "white")
  }
  
  # Create a summary stats table
  summary_stats <- data %>%
    summarise(
      avg_msgpack_encode_speedup = mean(json_encode_ms / msgpack_encode_ms),
      avg_msgpack_decode_speedup = mean(json_decode_ms / msgpack_decode_ms),
      avg_size_reduction = mean((1 - msgpack_size_bytes / json_size_bytes) * 100),
      max_data_size = max(data_size),
      total_tests = n()
    )
  
  cat("\n Summary Statistics:\n")
  cat(paste("Average encoding speedup:", round(summary_stats$avg_msgpack_encode_speedup, 2), "x\n"))
  cat(paste("Average decoding speedup:", round(summary_stats$avg_msgpack_decode_speedup, 2), "x\n"))
  cat(paste("Average size reduction:", round(summary_stats$avg_size_reduction, 1), "%\n"))
  cat(paste("Largest dataset tested:", scales::comma(summary_stats$max_data_size), "items\n"))
  cat(paste("Total benchmark tests:", summary_stats$total_tests, "\n"))
  
  cat("\n All plots generated successfully!\n")
  cat(" Check the 'graphs/' directory for PNG files\n")
}

# Run main function if script is executed directly
if (!interactive()) {
  main()
}