
# libs --------------------------------------------------------------------

library(tidyverse)
library(here)
library(scico)
library(ambient)


# Prelims -----------------------------------------------------------------

# Parameters

art_par <- list(
  seed = 2,
  n_paths = 500,
  n_steps = 80,
  sz_step = 200,
  sz_slip = 70
)


# setting up the canvas ---------------------------------------------------

set.seed(seed = art_par$seed)

state <- tibble(
  x = runif(n = art_par$n_paths, min = 0, max = 2), 
  y = runif(n = art_par$n_paths, min = 0, max = 2), 
  z = 0
)

# include the current state properties
state <- state %>% 
  mutate(
    path_id = 1:art_par$n_paths,
    step_id = 1
  )


# Keep track of the series of states
art_dat <- state


# Create the art in a loop ------------------------------------------------

stop_painting = F

while (stop_painting == F) {
  
  # make a step "magic step"
  step <- curl_noise(generator = gen_simplex, 
                     x = state$x, 
                     y = state$y,
                     z = state$z,
                     seed = c(1, 1, 1) * art_par$seed)
  
  # do some painting with the curl_noise params
  state <- state %>% 
    mutate(
      x = x + (step$x / 10000) * art_par$sz_step,
      y = y + (step$y / 10000) * art_par$sz_step,
      z = z + (step$z / 10000) * art_par$sz_slip, 
      step_id = step_id + 1
    )
  # Append the state iterative to the art_dat df
  art_dat <-  bind_rows(art_dat, state)
  
  # check the stopping statement
  current_step <- last(state$step_id)
  if (current_step >= art_par$n_steps) {
    stop_painting <- T
  }
  
}

# Draw on the canvas ------------------------------------------------------

pic <- ggplot(data = art_dat, 
              mapping = aes(x = x,
                            y = y, 
                            group = path_id,
                            color = step_id)) + 
  geom_path(size = 0.5, alpha = 0.5, show.legend = F) + 
  coord_equal() + theme_void() + 
scale_color_scico(palette = "grayC")

# save the plot -----------------------------------------------------------

pixels_wide = 3000
pixels_high = 3000
filename = "Scrawl.png"

ggsave(filename = filename,
       plot = pic,
       path = here("art"),
       width = pixels_wide/300,
       height = pixels_high/300,
       dpi = 300)










