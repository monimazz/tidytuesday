extrafont::loadfonts(device = "win")

library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(ggforce)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LIMPANDO OS DADOS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pegando os dados e colocando em data frames ----
tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artistas <- tuesdata$artists
obras <- tuesdata$artwork


base_unida <- left_join(obras, artistas, by = c("artistId" = "id"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Base somente com mulheres
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mulheres <- base_unida %>% 
  filter(gender == "Female") %>% 
  separate(placeOfBirth, into = c("Local", "Pais" ), sep = ", ")

mulheres %>% filter(is.na(Pais)) %>% tabyl(Local)

mulheres <- mulheres %>% 
  mutate(Pais = case_when(Local == 'Bangladesh' ~ 'Bangladesh',
                          Local == 'Bermondsey' ~   'Bermondsey', 
                          Local == "Brasil" ~"Brasil",  
                          Local == "Canada"  ~"Canada", 
                          Local == "Ceská Republika"   ~ "Ceská Republika",  
                          Local == "Colombia"     ~"Colombia",
                          Local == "Deutschland"     ~"Deutschland",
                          Local ==   "France"     ~"France",
                          Local ==   "Îran"     ~"Îran",
                          Local == "Jamaica"     ~"Jamaica",
                          Local ==     "Magyarország"    ~"Magyarország",
                          Local == "México"     ~"México",
                          Local == "Montserrat"  ~"Montserrat",   
                          Local ==  "Nihon"     ~"Nihon",
                          Local == "Österreich" ~ "Österreich",
                          Local == "Polska" ~ "Polska",
                          Local =="Schlesien" ~ "Schlesien",
                          Local =="Schweiz" ~ "Schweiz",
                          Local =="Singapore"  ~ "Singapore",
                          Local == "Taehan Min'guk"  ~ "Taehan Min'guk",
                          Local == "Türkiye" ~ "Türkiye",
                          Local == "United Kingdom" ~ "United Kingdom",
                          Local == "United States" ~ "United States",
                          Local ==   "Viet Nam"  ~ "Viet Nam",
                          Local == "Yisra'el" ~ "Yisra'el",
                          Local == "Zhonghua" ~ "Zhonghua",
                          TRUE ~ as.character(.$Pais)),
         Pais = as.character(Pais))

continentes <- readxl::read_excel("~/R/continentes.xlsx")

mulheres <- left_join(mulheres, continentes)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analisando os dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(packcircles) #grafico de circulos


# ================= % de genero entre artistas ==========
tabyl(artistas$gender)

library(packcircles)
data <- data.frame(group=paste(c("Mulher (14,75%)", "Homem (81,96%)", "NA (3,32%)")), value=c(14.75, 81.96, 3.32)) 

packing <- circleProgressiveLayout(data$value, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), alpha = 0.6) +
  geom_text(data = data, aes(x, y, size=value, label = group), color="black", family = "Montserrat") +
  theme_void() + 
  theme(legend.position="none",
        plot.background = element_rect(fill = "#F7F7F7", color = NA))+ 
  coord_equal() +
  scale_fill_gradient2()

ggsave("total_artistas_genero.png", device = "png", type = "cairo", height = 10, width = 12)


# ============ preseça de mulheres na coleçao ==========
data <- data.frame(group=paste(c("Mulher (3,94%)", "Homem (95%)", "NA (1%)")), value=c(3.94, 95.04, 1.01)) 

packing <- circleProgressiveLayout(data$value, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), alpha = 0.6) +
  geom_text(data = data, aes(x, y, size=value, label = group), color="black", family = "Montserrat") +
  theme_void() + 
  theme(legend.position="none",
        plot.background = element_rect(fill = "#F7F7F7", color = NA))+ 
  coord_equal() +
  scale_fill_gradient2()

ggsave("colecao_genero.png", device = "png", type = "cairo", height = 10, width = 12)


#============= 10 principais artistas ====================
base_unida %>% 
  count(artist, sort = TRUE) %>% 
  top_n(10) %>%
  mutate(artist = fct_reorder(artist, n)) %>% 
  ggplot(aes(artist, n,)) +
  geom_segment(aes(xend = artist, yend = 0),color = "#FBDB04", lineend = "round", size = 5) +
  coord_flip() +
  theme_minimal(base_family = "Montserrat", base_size = 12) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA),
        plot.title = element_text(size = 18, colour="#401A82"),
        plot.caption = element_text(face = "italic", size = 8, colour="#401A82"),
        axis.text.y =element_text(colour="#401A82")) +
  labs(title = "10 artistas mais presentes na coleção", caption = "Graphic: @monimazz\nData: TATE", x = "Artistas", y = "Total de obras", color = "")

ggsave("top_10art.png", device = "png", type = "cairo", height = 10, width = 12)


# ============ obras de turner ==============
library(waffle)

tunerr <- c(`Outro Artista`=43.1, `Turner`=56.9) # fiz a conta separado para identificar esse valor

waffle(tunerr/1, rows=8, size=1, colors=c("#A939B2", "#FBDB04")) +
  labs(title = "   Percentual de obras de Turner
       ", caption = "Cada quadrado é igual a 1%") +
  theme_void(base_family = "Montserrat", base_size = 12) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA))


ggsave("turner.png", device = "png", type = "cairo", height = 10, width = 12)



# ========== PRESENÇA NA COLEÇÃO DE MULHERES SEM O TURNER ============
data <- data.frame(group=paste(c("Mulher (9%)", "Homem (88%)", "NA (2%)")), value=c(9.14, 88.5, 2.34)) 

packing <- circleProgressiveLayout(data$value, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), alpha = 0.6) +
  geom_text(data = data, aes(x, y, size=value, label = group), color="black", family = "Montserrat") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal() +
  scale_fill_gradient2() +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA))


ggsave("colecao_semturner.png", device = "png", type = "cairo", height = 10, width = 12)


#============ Artistas mulheres no tate ao longo dos anos por aquisiçao==========

#TATE bolinhas por período de ano - ESTILO yayoi kusama
bolinhas_tate <-  mulheres %>% 
  filter(acquisitionYear != is.na(acquisitionYear)) %>%
  mutate(periodo = case_when(acquisitionYear<1908 ~ "1800",
                             acquisitionYear<1930 ~ "Anos 20",
                             acquisitionYear<1940 ~ "Anos 30",
                             acquisitionYear<1950 ~ "Anos 40",
                             acquisitionYear<1960 ~ "Anos 50",
                             acquisitionYear<1970 ~ "Anos 60",
                             acquisitionYear<1980 ~ "Anos 70",
                             acquisitionYear<1990 ~ "Anos 80",
                             acquisitionYear<2000 ~ "Anos 90",
                             acquisitionYear<2005 ~ "De 2000 a 2004",
                             acquisitionYear<2011 ~ "De 2005 a 2010",
                             acquisitionYear<2014 ~ "De 2011 a 2013")) %>%  
  count(periodo, Continentess, sort = TRUE)


# circle specifications
R = 1 
x0 = 0 
y0 = 0

# functions to generate random point within a circle with the specifications
gen_r <- function(n, radius = R) radius * sqrt(runif(n))
gen_theta <- function(n) runif(n) * 2 * pi

circulo_mulheres <- bolinhas_tate %>% 
  mutate(
    # Point generation
    r = map(n, gen_r),
    theta = map(n, gen_theta)
  ) %>% 
  unnest() %>% 
  mutate(
    # Convert to cartesian
    x = x0 + r * cos(theta),
    y = y0 + r * sin(theta)
  )

# data for ggforce::geom_circle() (speeds up geom rendering - ggplot tries to draw a circle for every data point otherwise)
circle <- data.frame(x0 = 0, y0 = 0, r = 1.1)

circulo_mulheres %>% 
  ggplot() +
  geom_circle(
    data = circle, 
    aes(x0 = x0, y0 = y0, r = r), 
    fill = "white", 
    color = "white", 
    alpha = 0.8
  ) +
  geom_point(
    aes(x = x, y = y),
    color = "#401A82",
    #position = "jitter",
    alpha = 0.5,
    size = 1.5
  ) +
  labs(title = "  Aquisição de obras de mulheres pelo Tate por período",caption = "Data: Tate/ @monimazz") +
  facet_wrap(periodo~.) +
  coord_fixed() +
  theme_void(base_family = "Montserrat") +
  scale_size(range = c(2, 14), guide = guide_none()) +
  xlim(c(-1.3, 1.3)) +
  ylim(c(-1.3, 1.13)) +
  theme(
    plot.background = element_rect(fill = "#FBDB04", color = NA),
    strip.text = element_text(size = 14),
    plot.caption = element_text(size = 10),
    plot.title = element_text(family = "Montserrat", face = "bold", size = 20, margin=margin(15,0,15,0))
  ) 

ggsave("tidytuesday_wk3_Tate_gender.png", device = "png", type = "cairo", height = 10, width = 12)


# =========== geo_point ano e regiao mulheres =======
mulheres %>% 
  group_by(acquisitionYear, Continentess) %>% 
  summarize(n = n(),
            Região = Região[n],
            Artista = artist[n]) %>% 
  ungroup() %>% 
  ggplot(aes(x = acquisitionYear, y = n, color = Continentess,
             Artista=Artista, Região=Região)) +
  geom_point() +
  theme_minimal(base_family = "Montserrat", base_size = 12) +
  labs(x = "Ano de aquisição pelo Tate", y = "Total de obras adquiridas", color = "Continente",
       title = "Aquisição de obras de mulheres pelo Tate") +
  scale_color_viridis_d() +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA))

ggsave("tempo_obras_mun.png", device = "png", type = "cairo", height = 8, width = 12)


library(plotly)
plot_mun <- ggplotly(p)

plot_mun


# ============ Quem representa as mulheres no Tate? (Estimativa) =============
base_unida <- base_unida %>% 
  mutate(titlemin = str_to_lower(title))

#principais termos
library(quanteda)
sem_turner <-  base_unida %>% 
  filter(artist != "Turner, Joseph Mallord William")


termos <- corpus(sem_turner$titlemin)
termos_tok <- tokens(termos, remove_numbers = TRUE, remove_punct = TRUE)
toks_nostop <- tokens_select(termos_tok, pattern = stopwords('en'),
                             selection = 'remove')
termos_dfm <- dfm(toks_nostop)  
fcmat <- fcm(termos_dfm)
feat <- names(topfeatures(fcmat, 400))

# obras com referencias a mulher no título


base_unida$obra_title [str_detect(base_unida$titlemin, "ariadne|elizabeth|women|girl|woman|^her|lady|^she|madame|dancer|venus|mother|sister|wife|signora|female|donna|^dame|maiden")] <- "Mulher"

base_unida <- base_unida %>% 
  mutate(obra_title = case_when(obra_title == "Mulher" ~ "Mulher",
                                TRUE ~ "Outro tema"))

mulher_titulo <- subset(genero, obra_title == "Mulher")


tabyl(mulher_titulo$gender) #92% homens, 6% mulheres e 1% NA



representacao <- c(`Homens`=92, `Mulheres`=6.5, `NA` = 1.5) # fiz a conta separado para identificar esse valor


#visu final ~~~~~~~~~~~~~~
waffle(representacao/1, rows=8, size=1, colors=c("#A939B2", "#FBDB04", "grey")) +
  labs(title = "  Estimativa de obras de representação de mulheres por gênero do artista
       ", caption = "Cada quadrado é igual a 1%") +
  theme_void(base_family = "Montserrat", base_size = 12) +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = NA))


ggsave("obras_representacao.png", device = "png", type = "cairo", height = 8, width = 12)
