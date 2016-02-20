ExUnit.start

Mix.Task.run "ecto.create", ~w(-r ElmTimer.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r ElmTimer.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(ElmTimer.Repo)

