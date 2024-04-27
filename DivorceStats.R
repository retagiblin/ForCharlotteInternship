library(tidycensus)
library(tidyverse)

one <- get_acs(
  geography = "tract",
  variables = "DP02_0030E",
  state = "NC"
)

view(one)

two <- get_acs(
  geography = "tract",
  variables = "DP02_0030PE",
  state = "NC"
)

view(two)

three <- get_acs(
  geography = "state",
  variables = "B12503_001E",
  state = "NC"
)
view(three)

four <- get_acs(
  geography = "tract",
  variables = "B99122_001E",
  state = "NC"
)

five <- get_acs(
  geography = "tract",
  variable = "S1201_C04_001E",
  state = "NC"
)
view(five)

six <- get_acs(
  geography = "tract",
  variable = "S1201_C04_016E",
  state = "NC"
)
view(six)

seven <- get_acs(
  geography = "tract",
  variable = "S1201_C05_001E",
  state = "NC"
)
view(seven)
