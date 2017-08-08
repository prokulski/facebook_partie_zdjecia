library(Rfacebook)
library(tidyverse)

# uzyskaj swoje numery na stronach Facebooka dla developerów
appID <- "xxxx"
appSecret <- "yyyy"

fb_oauth <- fbOAuth(app_id=appID, app_secret=appSecret, extended_permissions = TRUE)

# API key dla Face API - uzyskaj swój na stronie
# https://azure.microsoft.com/pl-pl/services/cognitive-services/face/
faceapi_key <- "xxxx"

# lista fanpages
fbpages <- c("pisorgpl", "PlatformaObywatelska", "Nowoczesna.oficjalnie",
             "KlubPoselskiKukiz15", "nowePSL", "partiarazem",
             "sojusz", "janusz.korwin.mikke")


get_fanpage_posts <- function(fb_page_name, n = 100, token) {
  # najnowsze posty z fanpage
  fb_page_posts <- getPage(fb_page_name, token=token, n=n)

  fb_page_posts <- fb_page_posts %>% select(-story, -link)
  return(fb_page_posts)
}

get_post_data <- function(post_id, token, n) {
  # dane o poście
  post <- getPost(post_id, token = token, n = n)

  if(!is.null(post$likes))
    post_likes <- post$likes %>%
      select(from_id, from_name) %>%
      mutate(message = "", type = "like")

  if(!is.null(post$comments))
    post_comments <- post$comments %>%
      select(from_id, from_name, message) %>%
      mutate(type = "comment")

  post_data <- rbind(post_likes, post_comments) %>% distinct()
  post_data$page_id <- post$post$from_id
  post_data$page_name <- post$post$from_name
  post_data$post_id <- post$post$id

  return(post_data)
}
#####



##### POBRANIE POSTÓW #####
# pobierz po 500 ostatnich postów z każdej ze stron
n_posts <- 500

fb_posts <- tibble()

for(i in 1:length(fbpages)) {
  cat(paste0("\n\nPobieram posty: ", fbpages[i], "\n"))

  tmp <- get_fanpage_posts(fbpages[i], n_posts, fb_oauth)
  fb_posts <- bind_rows(fb_posts, tmp)
}

rm(tmp)
#####


# tylko posty z liczbą komentarzy >= 3 kwartyl
fb_posts <- fb_posts %>%
  filter(comments_count >= quantile(comments_count, 0.75))



##### POBRANIE USER_ID LIKOW i KOMENTARZY #####

n_rows <- nrow(fb_posts)
users_actions <- tibble()

for(i in 1:n_rows) {
  cat(paste0("\r", i, "/", n_rows, ", post: ", fb_posts[i, "id"]))

  tmp <- get_post_data(fb_posts[i, "id"],
                       fb_oauth,
                       fb_posts[i, "likes_count"] + fb_posts[i, "comments_count"])

  cat(paste0(" - got ", nrow(tmp), " rows"))

  users_actions <- bind_rows(users_actions, tmp)

  cat(" - DONE\n")
}
rm(tmp)
#####


# najbardziej komentujący i lajkujący na stronach poszczególnych partii
most_active <- count(users_actions, page_name, type, from_name, sort=TRUE)


# znalezienie userów z największą liczbą interakcji
n_user_actions <- left_join(users_actions %>% # liczba interakcji na post per partia per osoba
                              count(from_id, page_name) %>%
                              ungroup() %>%
                              rename(user_id = from_id, user_actions = n),
                            fb_posts %>% # liczba wybranych postów per partia
                              count(from_name) %>%
                              ungroup() %>%
                              rename(n_party_posts = n),
                            by = c("page_name"="from_name")) %>%
  mutate(ratio = user_actions/n_party_posts)


# rozkład ratio
ggplot(n_user_actions) + geom_histogram(aes(ratio), binwidth=0.01) + facet_wrap(~page_name)

ggplot(n_user_actions) +
  geom_density(aes(ratio)) +
  geom_vline(xintercept = c(0.25, 0.5, 0.75, 1), color = "red", alpha=0.2) +
  facet_wrap(~page_name, scales="free_y") +
  scale_x_log10() +
  theme_minimal()




# userzy z ratio z top 1%, z wyłączeniem PSL (id=177473808919, jeden post)
most_active_users <- filter(n_user_actions,
                            ratio >= quantile(ratio, 0.99),
                            page_name != "Polskie Stronnictwo Ludowe")

count(most_active_users, page_name, sort=T)

most_active_users <- most_active_users %>% distinct(user_id, page_name)



#### inna wersja wyboru userów!

# po 100 najbardziej aktywnych userów z każdej z partii
most_active_users <- n_user_actions %>%
  group_by(page_name) %>%
  arrange(desc(ratio)) %>%
  mutate(n_row = row_number()) %>%
  filter(n_row <= 500) %>%
  ungroup() %>%
  select(user_id, page_name, ratio)



# info o userach z most_active_users
safe_getUsers <- purrr::safely(getUsers)


# dla Face API
# Do 20 transakcji na minutę, Bezpłatne transakcje miesięcznie: 30 000
library(httr)
face_api_url <- "https://api.projectoxford.ai/face/v1.0/detect?returnFaceAttributes=age,gender,smile,glasses"


users_info <- tibble()
for(i in 1:nrow(most_active_users)) {
  cat(paste0("\ri = ", i, "/", nrow(most_active_users)))

  user_id <- as.character(most_active_users[i, "user_id"])
  user_info_tmp <- safe_getUsers(user_id, token=fb_oauth)

  if(!is.null(user_info_tmp$result)) {
    user_info_tmp$result$page_name <- as.character(most_active_users[i, "page_name"])
    user_info_tmp$result$ratio <- as.numeric(most_active_users[i, "ratio"])

    # tutaj możemy zobaczyć od razu zdjęcie probilowe człowieka
    # ściągamy zdjęcie profilowe na dysk
    #       download.file(user_info_tmp$result$picture,destfile = "test.jpeg", mode = "wb")
    #       # wczytujemy zdjęcie
    #       jpg <- jpeg::readJPEG("test.jpeg")
    #       # rysujemy zdjęcie
    #       plot(0:1, 0:1, type = "n",
    #            main=user_info_tmp$result$name, xlab="", ylab="",
    #            axes = FALSE,  asp = 9/16)
    #       rasterImage(jpg, 0, 0, 1, 1)


    # używając API od Microsoftu możemy dowiedzieć się czegoś o twarzy
    result <- POST(face_api_url,
                   add_headers(.headers = c("Content-Type"="application/json",
                                            "Ocp-Apim-Subscription-Key"=faceapi_key)),
                   body = paste0("{ \"url\": \"", user_info_tmp$result$picture, "\" }"))

    API_Output <- content(result)
    if(length(API_Output) > 0) {
      #          print(API_Output[[1]]$faceAttributes$age)
      #          print(API_Output[[1]]$faceAttributes$gender)
      #          print(API_Output[[1]]$faceAttributes$smile)
      #          print(API_Output[[1]]$faceAttributes$glasses)

      # dodajemy dane z Face API do wyników z facebooka
      user_info_tmp$result$age <- API_Output[[1]]$faceAttributes$age
      user_info_tmp$result$pic_gender <- API_Output[[1]]$faceAttributes$gender
      user_info_tmp$result$smile <- API_Output[[1]]$faceAttributes$smile
      user_info_tmp$result$glasses <- API_Output[[1]]$faceAttributes$glasses
    }
    # dodajemy dane usera do listy
    users_info <- bind_rows(users_info, user_info_tmp$result)

    # czekamy 4 sekundy - żeby zmieścić się w limicie darmowych zapytań
    Sys.sleep(4)
  }
}

rm(user_info_tmp, user_id)


# standardowe zdjęcie profilowe?
std_photo <- c("https://scontent.xx.fbcdn.net/v/t1.0-1/s200x200/10354686_10150004552801856_220367501106153455_n.jpg?oh=6264a74255e689fe6989afa3694336b9&oe=5A26F750",
               "https://scontent.xx.fbcdn.net/v/t1.0-1/s200x200/1379841_10150004552801901_469209496895221757_n.jpg?oh=332800df942008df5016b83e65fc9ba1&oe=5A2E44F8")

users_info %>%
  mutate(def_picture = picture %in% std_photo) %>%
  count(page_name, def_picture) %>%
  mutate(p = 100 * n/sum(n)) %>%
  ungroup() %>%
  filter(def_picture) %>%
  arrange(p) %>%
  mutate(page_name = factor(page_name, levels = page_name)) %>%
  ggplot() +
  geom_bar(aes(page_name, p), stat = "identity",
           fill = "lightgreen", color = "black") +
  labs(title = "Najbardziej aktywni z domyślnym zdjęciem profilowym",
       x = "", y = "%") +
  coord_flip()



# wiek
users_info %>%
  filter(!is.na(age), age > 0) %>%
  mutate(pic_gender = ifelse(pic_gender == "male", "mężczyźni", "kobiety")) %>%
  ggplot() +
  geom_boxplot(aes(page_name, age, fill=pic_gender), show.legend = FALSE) +
  scale_fill_manual(values = c("kobiety" = "pink", "mężczyźni"="lightblue")) +
  labs(title = "Wiek najbardziej aktywnych użytkowników na fan page partii",
       x = "", y = "Wiek")




# kobiety czy mężczyźni?
users_info %>%
  filter(!is.na(age), age > 0) %>%
  mutate(pic_gender = ifelse(pic_gender == "male", "mężczyźni", "kobiety")) %>%
  count(page_name, pic_gender) %>%
  ungroup() %>%
  group_by(page_name) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(page_name, p, fill=pic_gender),
           color = "black", stat="identity") +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), alpha = 0.4) +
  scale_fill_manual(values = c("kobiety" = "pink", "mężczyźni"="lightblue")) +
  theme(legend.position = "bottom") +
  labs(title = "Podział najbardziej aktywnych użytkowników na fan page partii wg płci",
       x = "", y = "Udział procentowy danej płci", fill = "")



# grupy wiekowe i płeć
users_info %>%
  filter(!is.na(age), age > 0) %>%
  mutate(pic_gender = ifelse(pic_gender == "male", "mężczyźni", "kobiety")) %>%
  mutate(age = cut(age, breaks = c(0, 18, 25, 35, 45, 55, 70, 100))) %>%
  count(page_name, pic_gender, age) %>%
  ungroup() %>%
  group_by(page_name, pic_gender) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(age, p, fill=pic_gender),
           stat="identity", position = "dodge",
           color = "black", show.legend = FALSE) +
  facet_grid(page_name~pic_gender) +
  scale_fill_manual(values = c("kobiety" = "pink", "mężczyźni"="lightblue")) +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0)) +
  labs(title = "Podział najbardziej aktywnych użytkowników na fan page partii wg płci",
       x = "Grupa wiekowa", y = "Udział procentowy", fill = "")



# u kogo komentują osoby z danej grupy wiekowej o określonej płci?
users_info %>%
  filter(!is.na(age), age > 0) %>%
  mutate(pic_gender = ifelse(pic_gender == "male", "mężczyźni", "kobiety")) %>%
  mutate(age = cut(age, breaks = c(0, 18, 25, 35, 45, 55, 70, 100))) %>%
  count(page_name, pic_gender, age) %>%
  ungroup() %>%
  group_by(age, pic_gender) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(age, p, fill=page_name),
           color = "black", stat="identity", position = "dodge") +
  facet_wrap(~pic_gender, ncol=1) +
  theme(legend.position = "bottom") +
  labs(title = "Najbardziej aktywni użytkownicy na fan page partii",
       x = "Grupa wiekowa", y = "Procent z danej grupy", fill = "Partia")




# uśmiechają się?
users_info %>%
  filter(!is.na(age), age > 0) %>%
  mutate(pic_gender = ifelse(pic_gender == "male", "mężczyźni", "kobiety")) %>%
  ggplot() +
  geom_density(aes(smile, fill=pic_gender), alpha = 0.5) +
  facet_wrap(~page_name, ncol=4) +
  scale_fill_manual(values = c("kobiety" = "pink", "mężczyźni"="lightblue")) +
  labs(title = "Czy najbardziej aktywni użytkownicy na fan page partii się uśmiechają?",
       x = "Prawdopodobieństwo uśmiechu", y = "", fill = "Płeć") +
  theme(axis.text.y = element_blank())


# okularnicy
users_info %>%
  filter(!is.na(age), age > 0) %>%
  mutate(pic_gender = ifelse(pic_gender == "male", "mężczyźni", "kobiety")) %>%
  count(page_name, pic_gender, glasses) %>%
  ungroup() %>%
  group_by(page_name, pic_gender) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  mutate(glasses = factor(glasses,
                          level=c("NoGlasses", "ReadingGlasses",
                                  "Sunglasses", "SwimmingGoggles"),
                          labels = c("Brak okularów", "Okulary do czytania",
                                     "Okulary słoneczne", "Okulary do pływania"))) %>%
  ggplot() +
  geom_tile(aes(pic_gender, page_name, fill=p), color="white", show.legend = FALSE) +
  geom_text(aes(pic_gender, page_name, label=sprintf("%.1f%%", p))) +
  facet_wrap(~glasses) +
  scale_fill_gradient(low = "red", high="yellow")  +
  labs(title = "Okulary na zdjęciach profilowych najaktywniejszych użytkowników",
       x = "", y = "", fill = "")

