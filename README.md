# Dropbox for LispWorks

The `dropbox` package is a wrapper for the [CORE Dropbox REST API](https://www.dropbox.com/developers/core/docs). It uses [OAuth 2.0](http://oauth.net/) for authentication and allows for manipulation of Dropbox data.

This package requires the [`http`](https://www.github.com/massung/http) and [`json`](https://www.github.com/massung/json) packages (each has their own dependencies as well). Many of the Dropbox functions return RFC822 dates, which can be parsed into universal times with the [`date`](https://www.github.com/massung/date) package, but it isn't strictly required.

## Quickstart

There are special variables you'll need to set before you can use this tutorial: `*app-key*`, `*app-secret*`, and `*app-root*`.

	(setq dropbox:*app-key* "myappkey")
	(setq dropbox:*app-secret* "myappsecret")
	(setq dropbox:*app-root* :sandbox)

The variable `*app-root*` needs to be set to either `:dropbox` or `:sandbox`, based on the type of application you have (either full access or limited to just the `/Apps` folder). The default value is `:sandbox`.

First, you'll need to request a token.

	CL-USER > (dropbox-request-token)
	#<HQN-WEB::INTERNET-EXPLORER HINSTANCE 42>

This will open up a web browser window to Dropbox. If the user is already logged in it will prompt them to allow your application access. Otherwise it gives them the ability to log in first and then allow access.

Once access has been granted, a token will be displayed on the webpage, which must then be used to complete the process.

	CL-USER > (dropbox-request-access token)
	#<DROPBOX::ACCESS-TOKEN 200A5CFF>

Now that you have the token, you can use it to perform all sorts of fun Dropbox actions.

	CL-USER > (dropbox-account-info *)
	#<DROPBOX::ACCOUNT-INFO 200CB5D3>

That's it!

You can now download files, upload files, and more.

## Basic Functionality

TODO: