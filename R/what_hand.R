#' Assesses the strength of a 5-card poker hand.
#'
#' @param hand 5-card poker hand to evaluate.
#' @return What hand the cards make.
#' @examples
#' what_hand(deal_hand())
#' @export
what_hand <- function(hand) {

  ranks <- list(substr(hand[1], 1, nchar(hand[1]) - 1), substr(hand[2], 1, nchar(hand[2]) - 1), substr(hand[3], 1, nchar(hand[3]) - 1), substr(hand[4], 1, nchar(hand[4]) - 1),
             substr(hand[5], 1, nchar(hand[5]) - 1))
  ranks <- unlist(ranks, use.name=FALSE)
  suits <- list(substr(hand[1], nchar(hand[1]), nchar(hand[1])), substr(hand[2], nchar(hand[2]), nchar(hand[2])), substr(hand[3],
            nchar(hand[3]), nchar(hand[3])), substr(hand[4], nchar(hand[4]), nchar(hand[4])), substr(hand[5], nchar(hand[5]), nchar(hand[5])))
  suits <- unlist(suits, use.name=FALSE)

  for (i in 1:5) {
    if (ranks[i] == "A") ranks[i] = 1
    else
      if (ranks[i] == "K")ranks[i] = 13
      else
        if (ranks[i] == "Q")ranks[i] = 12
        else
          if (ranks[i] == "J") ranks[i] = 11
          else
            ranks[i] = as.numeric(ranks[i])
  }

  if (suits[1] == suits[2] && suits[2] == suits[3] && suits[3] == suits[4] && suits[4] == suits[5]) {
    suited = TRUE } else
    suited = FALSE


  ranks = sort(ranks, decreasing = FALSE)
  ranks <- as.numeric(ranks)
  if ((ranks[1] == 10 && ranks[2] == 11 && ranks[3] == 12 && ranks[4] == 13 && ranks[5] == 1) || (ranks[5] == ranks[4] + 1 && ranks[4] == ranks[3] + 1 && ranks[3] == ranks[2] + 1 &&
                                                                                                  ranks[2] == ranks[1] + 1)) {
      sequential = TRUE } else
    sequential = FALSE

  if (sequential && suited) {
    ret = "straight flush" } else {
    if (suited) {
      ret = "flush" } else {
      if (sequential)
        ret = "straight"
      }
    }

  if(!sequential && !suited) {

    if ((ranks[1] == ranks[2] && ranks[2] == ranks[3] && ranks[3] == ranks[4]) || (ranks[2] == ranks[3] && ranks[3] == ranks[4] && ranks[4] == ranks[5]))
        ret = "four of a kind"
    else {
      if ((ranks[1] == ranks[2] && ranks[2] == ranks[3] && ranks[4] == ranks[5]) || (ranks[1] == ranks[2] && ranks[3] == ranks[4] && ranks[4] == ranks[5]))
        ret = "full house"
      else {
        if ((ranks[1] == ranks[2] && ranks[3] == ranks[4]) || (ranks[2] == ranks[3] && ranks[4] == ranks[5]))
          ret = "two pair"
        else {
          count = 0
          for (i in 2:5)
          {
            if (ranks[i] == ranks[i-1])
              count = count + 1
          }
          if (count == 2)
            ret = "three of a kind"
          else if (count == 1)
            ret = "pair"
          else
            ret = "high card"
        }
      }
    }
  }

  ret = ret
}
