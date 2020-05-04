library(shiny)
shinyServer(
    function(input, output){
        output$rentalYield <- renderText({ calculateRentalYield(input$weekly_rent, input$price) })
        output$cashflow_per_year <- renderText({calculateYearlyCashflow(input$weekly_rent, input$water_per_quarter, input$management_fees, input$weekly_repayments)})
        output$cashflow_per_week <- renderText({calculateWeeklyCashflow(input$weekly_rent, input$water_per_quarter, input$management_fees, input$weekly_repayments)})
    }
)

calculateRentalYield <- function (weekly_rent, propertyPrice) 
{
    result <- weekly_rent * 52 / propertyPrice * 100
    return(round(result, digits = 2))
}

calculateYearlyCashflow <- function(weekly_rent, water, management_fees, weekly_repayments)
{
    result <- weekly_rent * 52 - (water) * 4 - management_fees * 52 - weekly_repayments * 52
    return(round(result, digits = 2))
}

calculateWeeklyCashflow <- function(weekly_rent, water, management_fees, weekly_repayments)
{
    result <- (weekly_rent * 52 - (water) * 4 - management_fees * 52 - weekly_repayments * 52) / 52
    return(round(result, digits = 2))
}
