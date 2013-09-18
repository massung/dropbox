# Dropbox for LispWorks

The `dropbox` package is a wrapper for the [CORE Dropbox REST API](https://www.dropbox.com/developers/core/docs). It uses [OAuth 2.0](http://oauth.net/) for authentication and allows for manipulation of Dropbox data.

## Quickstart

*NOTE: This tutorial assumes you have an application registered with Dropbox and that `*app-key*` and `*app-secret*` are set already.*

First, you'll need to request a token.

	CL-USER > (dropbox-request-token *app-key*)
	#<HQN-WEB::INTERNET-EXPLORER HINSTANCE 42>

This will open up a web browser window to Dropbox. If the user is already logged in it will prompt them to allow your application access. Otherwise it gives them the ability to log in first and then allow access.

Once access has been granted, a token will be displayed on the webpage, which must then be used to complete the process.

	CL-USER > (dropbox-request-access *app-key* *app-secret* token)
	#<DROPBOX::ACCESS-TOKEN 200A5CFF>
	
Now that you have the token, you can use it to perform all sorts of fun Dropbox actions.

	CL-USER > (dropbox-account-info *)
	#<DROPBOX::ACCOUNT-INFO 200CB5D3>

That's it!

You can now download files, upload files, and more.