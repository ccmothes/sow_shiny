# code to set up google authentication (only needs to be done once)
# from: https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4

library(googlesheets4)

# Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "ccmothes@gmail.com")

#NOTE: make sure .secrets file is included in deployment, so remove it from .gitignore
