
#' Publish an R Markdown Website
#'
#' Publish a website to RStudio Connect
#'
#' @inheritParams rsconnect::deploySite
#'
#' @param site_dir Directory containing website. Defaults to current working directory.
#' @param site_name Name for the site (names must be unique within an account). Defaults
#'   to the `name` provided by the site generator (or to the name of the site_dir if
#'   there is no `name` specified).
#' @param account Account to deploy application to. This parameter is only required for
#'   the initial deployment of an application when there are multiple accounts configured
#'   on the system.
#' @param method Publishing method (currently only "rsconnect" is available)
#' @param render `TRUE` to render the site locally before publishing.
#' @param launch_browser If `TRUE`, the system's default web browser will be launched
#'   automatically after the site is deployed. Defaults to `TRUE` in interactive sessions
#'   only.
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#' publish_site()
#' }
#'
#' @export
publish_site <- function(site_dir = ".", site_name = NULL,
                         method = c("rsconnect"), server = NULL, account = NULL,
                         render = TRUE, launch_browser = interactive()) {

  # resolve method
  method <- match.arg(method)

  if (identical(method, "rsconnect")) {

    # confirm that we have rsconnect
    if (!requireNamespace("rsconnect", quietly = FALSE)) {
      stop("The rsconnect package is required to publish websites. ",
           "Please install rsconnect with install.packages(\"rsconnect\")")
    }

    # check for non shinyapps.io accounts
    accounts <- rsconnect::accounts()
    accounts <- subset(accounts, server != "shinyapps.io")

    # if there is no server or account specified then see if we
    # can default the account
    if (is.null(server) && is.null(account)) {
      if (is.null(accounts) || nrow(accounts) == 0)
        stop("You must specify a server to publish the website to")
      else if (nrow(accounts) == 1) {
        account <- accounts$name
        server <- accounts$server
      }
    }

    # handle server
    if (!is.null(server) && is.null(account)) {

      # get a version of the server with the protocol (strip trailing slash)
      if (!grepl("^https?://", server))
        server_with_protocol <- paste0("https://", server)
      else
        server_with_protocol <- server
      server_with_protocol <- sub("/+$", "", server_with_protocol)

      # now strip the protocol if it's there
      server <- sub("^https?://", "", server_with_protocol)
      server_name <- server

      # ensure we have this server available
      accounts <- rsconnect::accounts()
      accounts <- subset(accounts, server == server_name)
      if (nrow(accounts) == 0) {

        # prompt
        message(sprintf("You do not currently have a %s publishing account ", server),
                        "configured on this system.")
        result = readline("Would you like to configure one now? [Y/n]: ")
        if (tolower(result) == "n")
          return(invisible())

        # create server if we need to
        servers <- rsconnect::servers()
        if (nrow(subset(servers, servers$name == server)) == 0) {

          rsconnect::addServer(sprintf("%s/__api__", server_with_protocol), server)
        }

        # connect user
        rsconnect::connectUser(server = server)

      }
      else if (nrow(accounts) == 1) {

        account <- accounts$name

      } else {

        stop("There is more than one account registered for ", server,
             "\nPlease specify which account you want to publish to.")

      }
    }

    # deploy site
    rsconnect::deploySite(
      siteDir = site_dir,
      siteName = site_name,
      account = account,
      server = server,
      render = if (render) "local" else "none",
      launch.browser = launch_browser
    )
  }
}
