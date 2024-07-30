return {
  "echasnovski/mini.move",
  version = "*",
  config = function()
    require("mini.move").setup({
      -- Move visual selection in Visual mode
      mappings = {
        left = "<M-S-h>",
        right = "<M-S-l>",
        down = "<M-S-j>",
        up = "<M-S-k>",
        -- Move current line in Normal mode
        line_left = "<M-S-h>",
        line_right = "<M-S-l>",
        line_down = "<M-S-j>",
        line_up = "<M-S-k>",
      },
    })
  end,
}
