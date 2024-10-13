library(tidyverse)
library(visitalaneysluverds)
library(scales)
library(metill)
theme_set(theme_metill())


d <- here::here("kaupmattur", "data", "tekjur_aldur_tiund.csv") |> 
  read_csv()

d |> 
  mutate(
    breyting = tekjur[ar == max(ar)] / tekjur - 1,
    medaltal_breyting =  medaltal[ar == max(ar)] /  medaltal - 1,
    .by = c(eining, aldur, tegund)
  ) |> 
  filter(
    ar == 2019
  ) |> 
  mutate(
    tegund = str_to_title(tegund),
    label = ifelse((eining == 10) & (aldur == "55 - 74 ára") & (tegund == "Heildartekjur"),
                   "Breyting meðaltekna aldurshóps", 
                   NA_character_)
  ) |> 
  rename(
    Ár = ar
  ) |> 
  filter(eining != 95) |> 
  ggplot(aes(eining, breyting + 1, ids = eining, group = eining)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = 0.4, linewidth = 0.4) +
  geom_hline(
    aes(yintercept = medaltal_breyting + 1, frame = Ár),
    alpha = 0.5,
    lty = 3,
    colour = "#e41a1c"
  ) +
  geom_text(
    aes(
      x = 45, 
      y = (medaltal_breyting + 1) * 1.01,
      label = label
    ),
    hjust = 0.5, 
    vjust = 1,
    colour = "#e41a1c"
  ) +
  geom_point() +
  geom_segment(aes(xend = eining, yend = 1), lty = 2, alpha = 0.5) +
  scale_x_continuous(
    breaks = c(seq(10, 90, by = 10), 99),
    # labels = \(x) {
    #   case_when(
    #     x == 10 ~ "10%\nLægstu tekjur",
    #     x == 50 ~ "Miðgildi\ntekna",
    #     x == 99 ~ "1%\nHæstu tekjur",
    #     TRUE ~ ""
    #   )
    # },
    labels = label_number(suffix = "%"),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = function(x) hlutf(x - 1),
    trans = "log10",
    guide = ggh4x::guide_axis_truncated()
  ) +
  facet_grid(cols = vars(aldur), rows = vars(tegund), scales = "free_y") +
  labs(
    x = "Tíundamörk",
    y = "Hlutfallsleg breyting frá árinu 2019",
    title = "Heildartekjur nær allra hópa hafa aukist frá 2019 til 2023",
    subtitle = "Hlutfallsleg breyting á verðbólguleiðréttum atvinnu- og heildartekjum eftir aldurs- og tekjutíundarhópi",
    caption = str_c(
      "10% þýðir tekjulægstu 10% | 99% þýðir tekjuhæstu 1% | 50% er miðgildi"
    )
  ) +
  theme(
    panel.spacing.x = unit(0.2, "cm")
  )

ggsave(
  filename = here::here("kaupmattur", "figures", "aukning.png"),
  height = 0.6 * 8, width = 8, scale = 1.6
)
