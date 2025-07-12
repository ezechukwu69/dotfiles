return {
  "saghen/blink.cmp",
  dependencies = {
    { "supermaven-inc/supermaven-nvim" },
    { "saghen/blink.compat" },
    { "nvim-orgmode/orgmode" },
    {"nvim-lua/plenary.nvim"},
    { 'Kaiser-Yang/blink-cmp-avante' },
    { 'L3MON4D3/LuaSnip',            version = 'v2.*' },
  },
  build = "cargo build --release",
  config = function()
    local opts = {
      snippets = {
        preset = "luasnip",
      },
      completion = {
        documentation = { auto_show = true },
        menu = {
          auto_show = true,
          draw = {
            treesitter = { 'lsp' },
            columns = {
              { "kind_icon", gap = 2 },
              { "label",     "label_description", gap = 2 },
              -- { "source_name" },
            }
          }
        },
      },
      signature = {
        enabled = true,
        trigger = {
          show_on_keyword = true,
          show_on_trigger_character = true,
          show_on_insert = true,
          show_on_insert_on_trigger_character = true,
        },
      },
      keymap = {
        preset = "enter",
        ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
        ['<C-e>'] = { 'hide', 'fallback' },
        ['<CR>'] = { 'accept', 'fallback' },
        ['<Tab>'] = { 'snippet_forward', 'fallback' },
        ['<S-Tab>'] = { 'snippet_backward', 'fallback' },
        ['<Up>'] = {},
        ['<Down>'] = {},
        ['<esc>'] = {},
        ['<C-p>'] = { 'select_prev', 'fallback_to_mappings' },
        ['<C-n>'] = { 'select_next', 'fallback_to_mappings' },
        -- ['<C-k>'] = { 'select_prev', 'fallback_to_mappings' },
        -- ['<C-j>'] = { 'select_next', 'fallback_to_mappings' },
        ['<C-b>'] = { 'scroll_documentation_up', 'fallback' },
        ['<C-f>'] = { 'scroll_documentation_down', 'fallback' },
        ['<C-s>'] = { 'show_signature', 'hide_signature', 'fallback' },
      },
      sources = {
        per_filetype = {
          org = {
            "orgmode",
          }
        },
        default = { "avante", 'lsp', 'path', 'snippets', 'buffer', 'digraphs', 'supermaven', "orgmode" },
        providers = {
          avante = {
            module = 'blink-cmp-avante',
            name = 'Avante',
            opts = {}
          },
          orgmode = {
            name = "Orgmode",
            module = "orgmode.org.autocompletion.blink",
            fallbacks = { "buffer" }
          },
          -- orgmode = {
          --   name = "orgmode",
          --   module = "blink.compat.source",
          -- },
          supermaven = {
            name = "Supermaven",
            module = "blink.compat.source",
            score_offset = 100,
            async = true,
          },
          digraphs = {
            -- IMPORTANT: use the same name as you would for nvim-cmp
            name = 'digraphs',
            module = 'blink.compat.source',

            -- all blink.cmp source config options work as normal:
            score_offset = -3,

            -- this table is passed directly to the proxied completion source
            -- as the `option` field in nvim-cmp's source config
            --
            -- this is NOT the same as the opts in a plugin's lazy.nvim spec
            opts = {
              -- this is an option from cmp-digraphs
              cache_digraphs_on_start = true,

              -- If you'd like to use a `name` that does not exactly match nvim-cmp,
              -- set `cmp_name` to the name you would use for nvim-cmp, for instance:
              -- cmp_name = "digraphs"
              -- then, you can set the source's `name` to whatever you like.
            },
          },
        },
      },
    }
    require("blink.cmp").setup(opts)
  end
}
