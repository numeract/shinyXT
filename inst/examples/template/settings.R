# dispatcher for specific user settings which are in dir settings/

# !diagnostics suppress=.,


# Since individual user settings might replace already defined values,
# source this file after sourcing constants.R and helper.R
# Ideally, the user settings should not (re)define R functions


# definitions ----
.ENV <- list(
    # Pairs of MD5 hashes and associated settings file
    # Add your hash using `digest::digest(Sys.info())`
    # Remember to update the hash if the environment changes
    #
    # Using the hash makes the environment characteristics secure,
    # while avoiding defining a default configuration
    # This setup allows for the same file, different contents using .gitignore
    
    "382a8f229a22a82b1a47942a4ceda553" = "settings_mb.R",
    
    # default settings file if no hash found
    "unknow_machine" = "settings_default.R"
)


# load settings ----
.settings_file <- .ENV[[digest::digest(Sys.info())]]
if (is.null(.settings_file)) {
    cat(":: Unrecognized environment, looking for the default settings file! ::\n")
    .settings_file <- .ENV[["unknow_machine"]]
}

if (is.null(.settings_file)) {
    cat(":: No default settings file found - skipping settings! ::\n")
    # alternatively, we can force a stop if no settings
    # stop(":: No default settings file - skipping settings! ::")
} else {
    .settings_file <- paste0("settings/", .settings_file)
    if (file.exists(.settings_file)) {
        cat(":: Loading settings file:", .settings_file, "::\n")
        source(.settings_file)
    } else {
        cat(":: Missing settings file:", .settings_file, "- skipping settings! ::\n")
    }
}


# cleanup ----
rm(.ENV, .settings_file)


# checks ----
# this section depends on what is expected to be loaded from settings
