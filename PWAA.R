library(dplyr)
library(na.tools)
library(ggimage)
library(nflfastR)
library(ggrepel)
library(ggimage)
library(ggtext)
library(mgcv)
library(scales)
library(ggforce)
library(gt)
library(remotes)

seasons <- 2013:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(old_game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

run_epa <- pbp_rp %>%
  filter(qb_dropback == 0) %>%
  filter(qb_scramble == 0) %>%
  filter(rush_attempt==1) %>%
  group_by(posteam, season) %>%
  summarize(rush_epa = mean(epa, na.rm=T))

def_epa <- pbp_rp %>%
  group_by(defteam, season) %>%
  summarize(def_epa = mean(epa, na.rm = T))

names(run_epa)[names(run_epa) == "posteam"] <- "team"
names(def_epa)[names(def_epa) == "defteam"] <- "team"

help_epa <- merge(run_epa, def_epa, by = c("team", "season"))

help_epa <- help_epa %>%
  mutate(total_help = rush_epa - def_epa) %>%
  arrange(total_help)

standings <- read.csv("~/QB WAR/preds_2020.csv")

standings <- standings %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

filtered_standings <- standings %>%
  select(season, team, wins, losses, ties, pct, sos) %>%
  filter(season >= 2013)

filtered_standings <- filtered_standings %>%
  mutate(total_wins = ifelse(ties==1, wins + 0.5, wins)) %>%
  select(season, team, total_wins, sos)

#Download the CSV and fix 2020 wins/losses and re-upload
filtered_standings$season <- as.numeric(filtered_standings$season)
help_epa$season <- as.numeric(help_epa$season)

standings_with_epa <- filtered_standings %>%
  left_join(help_epa, by = c("team", "season"))

ggplot(standings_with_epa, aes(x=total_help, y=total_wins)) + 
  geom_point() +
  geom_abline()

#Download standings_with_epa to manually add PFF grades
write.csv(standings_with_epa, "standings_with_epa2.csv")

every_year_help <- standings_with_epa %>% 
  group_by(team) %>%
  summarize(final_help = mean(total_help)) %>% 
  arrange(desc(final_help)) %>%
  filter(!is.na(final_help))

qb_dropbacks <- pbp_rp %>%
  filter(qb_dropback == 1)
mean(qb_dropbacks$epa) #0.05

#Upload CSV with PFF grades now included
pwaa_with_teams <- filtered_standings
pwaa_no_teams <- filtered_standings %>%
  select(-season, -team)

pwaa_no_teams %>% 
  cor(use="complete.obs") %>%
  round(2)

fit <- lm(total_wins ~ rush_epa + def_epa + pass_block_grade +  receiving_grade + sos, data = pwaa_no_teams)
summary(fit)

preds <- predict(fit, pwaa_with_teams) %>% 
  as_tibble() %>%
  rename(proj_wins = value) %>%
  round(1) %>%
  bind_cols(
    pwaa_with_teams %>% select(team, season, total_wins)
  )

preds <- preds %>%
  mutate(pwaa = total_wins - proj_wins) %>%
  filter(!is.na(pwaa))

teams_colors_logos <- teams_colors_logos

preds2 <- preds %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) 
preds2 <- preds2 %>%
  select(proj_wins, team, season, total_wins, pwaa, team_color, team_color2, team_logo_espn)
preds3 <- preds2 %>%
  left_join(headshots, by = c('season', 'team')) 


preds_2020 <- preds3 %>%
  filter(season == 2020) %>%
  arrange(desc(pwaa))

preds_2019 <- preds2 %>%
  filter(season == 2019) %>%
  arrange(desc(pwaa))

preds_2018 <- preds2 %>%
  filter(season == 2018) %>%
  arrange(desc(pwaa))
      
all_pred_years <- preds %>%
  group_by(team) %>%
  summarize(mean_proj_wins = mean(proj_wins),
            mean_wins = mean(total_wins),
            mean_pwaa = mean(pwaa)) %>%
  arrange(desc(mean_proj_wins))

remotes::install_github("jthomasmock/espnscrapeR")

qb_epa <- qb_dropbacks %>%
  group_by(passer, season, posteam) %>%
  summarize(plays = n(),
            qb_epa = mean(epa, na.rm=T)) %>%
  filter(plays > 50)

qb_epa_top_plays <- qb_epa %>%
  group_by(season, posteam) %>%
  top_n(1, plays)

qbr_data_20 <- espnscrapeR::get_nfl_qbr(2020)
qbr_data_19 <- espnscrapeR::get_nfl_qbr(2019)
qbr_data_18 <- espnscrapeR::get_nfl_qbr(2018)
qbr_data_17 <- espnscrapeR::get_nfl_qbr(2017)
qbr_data_16 <- espnscrapeR::get_nfl_qbr(2016)
qbr_data_15 <- espnscrapeR::get_nfl_qbr(2015)
qbr_data_14 <- espnscrapeR::get_nfl_qbr(2014)
qbr_data_13 <- espnscrapeR::get_nfl_qbr(2013)

qbr_data_all <- rbind(qbr_data_13, qbr_data_14, qbr_data_15, qbr_data_16, 
                      qbr_data_17, qbr_data_18, qbr_data_19, qbr_data_20) %>%
  select(season, team, short_name, headshot_href)

qbr_data_all <- qbr_data_all %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

names(qbr_data_all)[names(qbr_data_all) == "short_name"] <- "passer"
names(qb_epa_top_plays)[names(qb_epa_top_plays) == "posteam"] <- "team"

qb_epa_top_plays$season <- as.integer(qb_epa_top_plays$season)
qbr_data_all$season <- as.integer(qbr_data_all$season)

qbr_data_all <- qbr_data_all %>%
  distinct(passer, .keep_all = TRUE) %>%
  select(-season, -team)

qb_epa_headshots <- qb_epa_top_plays %>%
  left_join(qbr_data_all, by = "passer")

write.csv(qb_epa_headshots, "qb_epa_headshots.csv")
write.csv(qbr_data_all, "qbr_data_all.csv")

headshots <- read.csv("~/Downloads/headshots.csv")

################################# G R A P H S ################################
preds_2020 <- preds_2020 %>%
  mutate(rank = row_number())

asp_ratio <- 1.618 

link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

bar_plot <- preds_2020 %>% 
  mutate(label = link_to_img(headshot_href),
         rank = as.integer(rank)) %>% 
  ggplot() +
  geom_col(
    aes(
      x = rank, y = pwaa,
      fill = team_color, color = team_color2
    ),
    width = 0.4
  ) + 
  scale_color_identity(aesthetics =  c("fill", "color")) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1, seq(5, 30, by = 5)), limits = c(0.5, 34)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = NULL,
       y = "PWAA\n",
       title = "Passer Wins Above Average (PWAA) | 2020 Season",
       subtitle = "PWAA is a linear regression model that simulates an average quarterback with every team's run game, defense, pass blocking and receiving",
       caption = "By Tej Seth | @mfbanalytics") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

bar_plot

qb_col_img <- bar_plot +
  geom_image(
    aes(
      x = rank, y = pwaa,
      image = headshot_href
    ),
    size = 0.06
  )

qb_col_img

ggsave(
  "pwaa-1.png", qb_col_img, 
  height = 10, width = 16, dpi = "retina"
)
#################################################

preds_2020 %>%
  ggplot() +
  geom_link(
    mapping = aes(x = proj_wins, y = rank, xend = total_wins, yend = rank, size = 2, color = team_color)
  ) +
  theme_bw() +
  scale_colour_identity() +
  geom_image(aes(x = proj_wins, y = rank, image = team_logo_espn), size = 0.04, asp = 16/9) +
  geom_image(aes(x = total_wins, y = rank, image = headshot_href), size = 0.04, asp = 16/9) +
  labs(
    x = "Team Wins",
    y = "",
    title = "Each Team's Passer Wins Over Average",
    subtitle = "The team logo is for a team's wins with an average quarterback and the face of each quarterback is how many wins they added",
    caption = "By Tej Seth | @mfbanalytics using code from @benbbaldwin"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10))

ggsave('pwaa-2.png', dpi=300, height=9*.8, width=16*.8)

###########################################################
scatter_plot <- preds_2020 %>% 
  mutate(label = link_to_img(headshot_href),
         rank = as.integer(rank)) %>% 
  ggplot() +
  geom_smooth(aes(x = pwaa, y = qb_epa), method = "lm", color = "grey") +
  ggrepel::geom_text_repel(
    aes(x = pwaa, y = qb_epa, label = passer),
    box.padding = 0.5, fontface = "bold", size = 6
  ) + 
  geom_point(
    aes(x = pwaa, y = qb_epa, size = plays, fill = team_color, color = team_color2), 
    shape = 21
  ) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  scale_size(name = "Plays") +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Passer Wins Above Average",
       y = "QB's EPA/Play",
       title = "PWAA and EPA/Play Are Correlated",
       subtitle = "PWAA is a linear regression model that simulates an average quarterback with every team's run game, defense, pass blocking and receiving",
       caption = "By Tej Seth | @mfbanalytics using code from @thomas_mock") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) 

scatter_plot

ggsave(
  "pwaa-3.png", scatter_plot, 
  height = 10, width = 16, dpi = "retina"
)
############################################################
all_pred_years <- all_pred_years %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

all_pred_years %>%
  ggplot(aes(x = mean_proj_wins, y = mean_wins)) +
  geom_hline(yintercept = mean(all_pred_years$mean_wins), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(all_pred_years$mean_proj_wins), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  labs(x = "Wins With Average Quarterback Per Year",
       y = "Actual Wins Per Year",
       title = "How Each Team Has Peformed Since 2013",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = 9.8, y = 9.2, label = "Good Franchise \n Good QB Play", color = "blue") +
  annotate("text", x = 9.8, y = 6, label = "Good Franchise \n Bad QB Play", color = "blue") +
  annotate("text", x = 6, y = 7, label = "Bad Franchise \n Bad QB Play", color = "blue") +
  annotate("text", x = 6, y = 9, label = "Bad Franchise \n Good QB Play", color = "blue")

ggsave('pwaa-4.png')

#####################################################################
every_qb_pef <- merge(headshots, preds, by = c("season", "team"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

qb_pwaa_stats <- every_qb_pef %>%
  group_by(passer, headshot_href) %>%
  summarize(seasons = n(),
            mean_pwaa = mean(pwaa, na.rm = T),
            total_pwaa = sum(pwaa, na.rm = T),
            mean_proj_wins = mean(proj_wins, na.rm = T),
            most_freq_team = Mode(team)) %>%
  filter(seasons > 2) %>%
  arrange(desc(total_pwaa))

qb_pwaa_stats <- qb_pwaa_stats %>%
  mutate(rank = row_number())

qb_pwaa_stats <- qb_pwaa_stats %>%
  left_join(teams_colors_logos, by = c('most_freq_team' = 'team_abbr'))

side_plot <- qb_pwaa_stats %>% 
  mutate(label = link_to_img(headshot_href),
         rank = as.integer(rank)) %>% 
  ggplot() +
  geom_col(
    aes(
      x = total_pwaa, y = fct_reorder(passer, total_pwaa),
      fill = team_color, color = team_color2
    ),
    width = 0.4
  ) + 
  scale_color_identity(aesthetics =  c("fill", "color")) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Total Passer Wins Above Average",
       y = "",
       title = "Total PWAA by Each Quarterback Since 2013",
       subtitle = "Minimum of 3 seasons played",
       caption = "By Tej Seth | @mfbanalytics") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 14)
  ) 

side_plot

ggsave("pwaa-5.png")
#####################################################################
top_15 <- every_qb_pef %>% arrange(desc(pwaa)) %>% slice(1:15)

top_15 <- top_15 %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

top_15 <- top_15 %>%
  mutate(rank = row_number())

top_15 <- top_15 %>%
  mutate(text_label = (total_wins + proj_wins) / 2)

top_15 %>%
  ggplot() +
  geom_link(
    mapping = aes(x = proj_wins, y = rank, xend = total_wins, yend = rank, size = 2, color = team_color)
  ) +
  theme_bw() +
  scale_colour_identity() +
  geom_image(aes(x = proj_wins, y = rank, image = team_logo_espn), size = 0.04, asp = 16/9) +
  geom_image(aes(x = total_wins, y = rank, image = headshot_href), size = 0.04, asp = 16/9) +
  geom_text(aes(x = text_label, y = rank, label = season), color = "white") +
  labs(
    x = "Team Wins",
    y = "",
    title = "The Top 15 Seasons in PWAA Since 2013",
    subtitle = "The team logo is for a team's wins with an average quarterback and the face of each quarterback is how many wins they added",
    caption = "By Tej Seth | @mfbanalytics using code from @benbbaldwin"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10))

ggsave('pwaa-6.png', dpi=300, height=9*.8, width=16*.8)
#########################################################
mahomes <- every_qb_pef %>%
  filter(passer == "P.Mahomes")

mahomes <- mahomes %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

mahomes %>%
  ggplot() +
  theme_bw() + 
  scale_colour_identity() +
  geom_line(aes(x=season, y=proj_wins, color = team_color)) +
  geom_line(aes(x=season, y=total_wins, color = team_color2)) +
  geom_image(aes(x=season, y=proj_wins, image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_image(aes(x=season, y=total_wins, image = headshot_href), size = 0.05, asp = 16/9) +
  labs(
    x = "Season",
    y = "Wins",
    title = "Patrick Mahomes Has Taken Good Team and Made Them Elite",
    subtitle = "A team's logo is how many wins they'd have with an average quarterback and the quarterback's face is how many wins they actually got to",
    caption = "By Tej Seth | @mfootballanalytics"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 14)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
ggsave('pwaa-7.png', dpi = 300)

#########################################################
tab_data <- preds_2020 %>% 
  mutate(RK = rank(desc(pwaa)),
         RK = as.integer(RK)) %>% 
  select(RK, passer, headshot_href, proj_wins, total_wins, pwaa)

tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot_href)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>% 
    cols_label(
      RK = "Rank",
      passer = "Quarterback",
      headshot_href = "",
      proj_wins = "Avg. QB Wins",
      total_wins = "Actual Wins",
      pwaa = "PWAA") %>%
    data_color(
      columns = vars(pwaa),
      colors = scales::col_numeric(
        palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"),
        domain = c(-5, 5)
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(RK, passer)
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) %>%
    opt_table_font(
      font = list(
        default_fonts()
      )
    ) 
}

gt_tab1 <- tab_data %>% 
  slice(1:16) %>% 
  tab_function()
gt_tab1

gtsave(gt_tab1, "gt-tab1.png")

gt_tab2 <-  tab_data %>% 
  slice(17:32) %>% 
  tab_function() %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "black",
      weight = px(3)
    ),
    locations = 
      list(
        cells_body(
          columns = 1
        ),
        cells_column_labels(1)
      )
  )
gtsave(gt_tab2, "gt-tab2.png")

img1 <- magick::image_read("gt-tab1.png")
img2 <- magick::image_read("gt-tab2.png")

img3 <- magick::image_append(c(img1, img2))
img3

###########################################################################
passer_stats <- qb_dropbacks %>%
  group_by(posteam, season) %>%
  summarize(qb_epa = mean(epa))

names(passer_stats)[names(passer_stats) == "posteam"] <- "team"
filtered_standings$season <- as.numeric(filtered_standings$season)
passer_stats$season <- as.numeric(passer_stats$season)

qb_standings <- filtered_standings %>%
  left_join(passer_stats, by = c("team", "season"))

fit2 <- lm(total_wins ~ rush_epa + def_epa + pass_block_grade +  receiving_grade + sos + qb_epa, data = qb_standings)
summary(fit2)

qb_standings %>% 
  select(-season, -team, -avg_qb_epa) %>%
  cor(use="complete.obs") %>%
  round(2)

corr <- qb_standings %>% 
  select(-season, -team, -avg_qb_epa)

corrgram(corr, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Blue ")

ggcorplot(corr, hc.order = TRUE,
          type = "lower",
          lab = TRUE,
          lab_size= 3,
          method = "circle",
          )