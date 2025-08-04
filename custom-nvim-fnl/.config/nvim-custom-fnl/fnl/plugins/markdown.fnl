(vim.pack.add [ "https://github.com/MeanderingProgrammer/render-markdown.nvim" ])

(local markdown (require "render-markdown"))

(markdown.setup {
    :file_types ["markdown" "Avante"]
})
