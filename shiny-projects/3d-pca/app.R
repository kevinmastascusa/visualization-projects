# AUTHOR : KEVIN MASTASCUSA
# DATE : FRI SEP 6
# TIME 2:57 PM
library(shiny)
library(plotly)

# Load the iris dataset
data <- iris

# Select relevant columns for PCA and scale
pca_data <- scale(data[, 1:3])

# Perform PCA
pca_result <- prcomp(pca_data)
principal_components <- pca_result$x
variances <- pca_result$sdev^2

# Calculate percentage of variance explained by each PC
percent_variance <- round(100 * variances / sum(variances), 1)

# User Interface
library(shiny)
library(plotly)

# User Interface
ui <- fluidPage(
    titlePanel("3D PCA Visualization of Iris Dataset"),
    sidebarLayout(
        sidebarPanel(
            h4("Principal Component Analysis"),
            h3("Created By: Kevin Mastascusa"),
            p("This visualization explores the Iris dataset using PCA. Each point represents an Iris flower, colored by its species. The axes represent the principal components, which capture the most variance in the data."),
            p("Hover over points to see their species and PC values."),
            p(paste0("PC1 explains ", percent_variance[1], "% of the variance.")),
            p(paste0("PC2 explains ", percent_variance[2], "% of the variance.")),
            p(paste0("PC3 explains ", percent_variance[3], "% of the variance."))
        ),
        mainPanel(
            plotlyOutput("plot3d")
        )
    )
)

# Server Logic
server <- function(input, output) {
    output$plot3d <- renderPlotly({
        plot_ly(
            x = principal_components[,1],
            y = principal_components[,2],
            z = principal_components[,3],
            type = "scatter3d",
            mode = "markers",
            color = as.numeric(data$Species),
            colors = c("#E41A1C", "#377EB8", "#4DAF4A"),
            text = paste("Species:", data$Species, "<br>",
                         "PC1:", round(principal_components[,1], 2), "<br>",
                         "PC2:", round(principal_components[,2], 2), "<br>",
                         "PC3:", round(principal_components[,3], 2)),
            hoverinfo = "text",
            marker = list(size = 6)
        ) %>%
            layout(
                scene = list(
                    xaxis = list(title = "PC1"),
                    yaxis = list(title = "PC2"),
                    zaxis = list(title = "PC3")
                ),
                # Add a legend to the plot
                legend = list(title = list(text = "Iris Species"))
            )
    })
}

shinyApp(ui = ui, server = server)

# Server Logic
server <- function(input, output) {
    output$plot3d <- renderPlotly({
        plot_ly(
            x = principal_components[,1],
            y = principal_components[,2],
            z = principal_components[,3],
            type = "scatter3d",
            mode = "markers",
            color = as.numeric(data$Species),
            colors = c("#E41A1C", "#377EB8", "#4DAF4A"), # Distinctive color palette
            text = paste("Species:", data$Species, "<br>",
                         "PC1:", round(principal_components[,1], 2), "<br>",
                         "PC2:", round(principal_components[,2], 2), "<br>",
                         "PC3:", round(principal_components[,3], 2)),
            hoverinfo = "text",
            marker = list(size = 6)
        ) %>%
            layout(
                scene = list(
                    xaxis = list(title = "PC1"),
                    yaxis = list(title = "PC2"),
                    zaxis = list(title = "PC3")
                ),
                legend = list(title = list(text = "Iris Species"))
            )
    })
}

shinyApp(ui = ui, server = server)
