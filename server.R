#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlotly({

        # generate bins based on input$bins from ui.R
        set.seed(input$seed)

        # draw the histogram with the specified number of bins
        getPlot(
            example = input$example,
            showObservations = input$showObservations,
            m = input$m,
            sigma = input$sigma,
            T1 = input$T1
        )

    })
    
    output$formula <- renderUI({
        switch(
            input$example,
            "noisy observations of a constant process" = withMathJax(
               "$$dX_t=0, X_0 \\sim N(0, \\sigma^2)$$
                $$dZ_t=X_tdt + mdV_t, Z_0 = 0$$
                $$\\hat{X}_t = \\frac{\\sigma^2}{m^2+\\sigma^2 t} Z_t$$
                $$S(t) = \\frac{\\sigma^2 m^2}{m^2 + \\sigma^2 t}$$"
            ),
            "noisy observations of a Brownian motion" = withMathJax(
                "$$dX_t=dU_t, X_0 = 0 $$
                $$dZ_t=X_tdt + dV_t$$
                $$Z_t = \\int_0^t H_s ds$$
                $$\\hat{X}_t = \\frac{1}{\\cosh(t)} \\int_0^t \\sinh(s) H_s ds$$
                $$S(t) = \\tanh{t}$$"
            ),
            "noisy observations of population growth" = withMathJax(
               "$$X_t = \\exp \\left( rt \\right) X_0, X_0 \\sim N(\\mu, \\sigma^2)$$
                $$Z_t = X_0 \\left( \\frac{1}{r} \\exp(rt) - 1 \\right) + m dV_t$$
                $$\\hat{X}_t = \\exp(-rt) \\left( \\int\\limits_0^t 2r \\exp(rs)H_sds + \\mu \\right)$$
                $$S(t) = \\sigma^2$$
                $$\\mu = 1, \\ r = \\frac{\\sigma^2}{2 m^2}$$"
            ),
            "constant model for exponential growth" = withMathJax(
               "$$X_t = \\exp \\left( rt \\right) X_0, X_0 \\sim N(\\mu, \\sigma^2)$$
                $$Z_t = X_0 \\left( \\frac{1}{r} \\exp(rt) - 1 \\right) + m dV_t$$
                $$\\hat{X}_t = \\frac{1}{1+t} Z_t ds$$
                $$\\mu = 1, \\ r = \\frac{\\sigma^2}{2 m^2}$$"
            )
        )
    })

})
