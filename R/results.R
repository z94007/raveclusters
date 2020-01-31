text_result <- function(result){
  baselined = result$get_value('baselined', NULL)
  
  
  validate(
    need(!is.null(baselined), message = 'Not baselined')
  )
  # shiny:::reactiveStop('This is an error message', c("validation"))
  
  image(baselined$get_data()[1,,,1])
}