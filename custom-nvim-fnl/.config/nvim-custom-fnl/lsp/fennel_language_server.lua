return {
    -- replace it with true path
    cmd = {'fennel-language-server'},
    filetypes = {'fennel'},
    -- source code resides in directory `fnl/`
    root_markers = { ".git", "init.lua" },
    settings = {
      fennel = {
        workspace = {
          -- If you are using hotpot.nvim or aniseed,
          -- make the server aware of neovim runtime files.
          library = vim.api.nvim_list_runtime_paths(),
        },
        diagnostics = {
          globals = {'vim'},
        },
      },
    },
}
