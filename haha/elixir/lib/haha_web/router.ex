defmodule HaHaWeb.Router do
  use HaHaWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", HaHaWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
  end

  scope "/openc2", HaHaWeb do
    pipe_through :api
    post "/", OC2Controller, :command
  end
end
