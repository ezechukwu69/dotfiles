return {
  "stevearc/overseer.nvim",
  config = function(opts)
    local overseer = require("overseer")
    overseer.setup(opts)
    overseer.register_template({
      name = "Dart generate",
      description = "Dart build_runner watch (Build runner)",
      builder = function(params)
        return {
          cmd = "dart",
          args = { "run", "build_runner", "watch", "--delete-conflicting-outputs" },
        }
      end,
    })
  end,
}
