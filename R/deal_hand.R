#' Simulates dealing a 5-card poker hand.
#'
#' @return Generates a random five-card poker hands.
#' @examples
#' poker_hand()
#' @export
deal_hand <- function() {
  deck <- cardDeck <- c(outer(c("A",2:10,"J","Q","K"),
                              c("\u2660","\u2665","\u2666","\u2663"), paste0))
  hand <- sample(deck, 5, replace = FALSE)
}
