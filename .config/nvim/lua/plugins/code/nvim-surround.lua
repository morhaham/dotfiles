return {
  "kylechui/nvim-surround",
  version = "^3.1.1", -- Use for stability; omit to use `main` branch for the latest features
  event = "VeryLazy",
  config = function()
    require("nvim-surround").setup({
      keymaps = {
        insert = "<C-g>s", -- Insert mode
        insert_line = "<C-g>S", -- Insert mode (line)
        normal = '<leader>"', -- Normal mode (was "ys")
        normal_cur = '<leader>""', -- Normal mode (cur line, was "yss")
        normal_line = '<leader>"L', -- Normal mode (line, was "yS")
        visual = '<leader>"v', -- Visual mode (was "S")
        visual_line = '<leader>"V', -- Visual mode (line, was "gS")
        delete = '<leader>"d', -- Delete surrounding (was "ds")
        change = '<leader>"c', -- Change surrounding (was "cs")
      },
    })
  end,
}
