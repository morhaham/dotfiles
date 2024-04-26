return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require("harpoon")
    harpoon:extend({
      UI_CREATE = function(cx)
        vim.keymap.set("n", "<C-v>", function()
          harpoon.ui:select_menu_item({ vsplit = true })
        end, { buffer = cx.bufnr })

        vim.keymap.set("n", "<C-x>", function()
          harpoon.ui:select_menu_item({ split = true })
        end, { buffer = cx.bufnr })
      end,
    })
    harpoon.setup()
  end,
  keys = {
    {
      "<C-a>",
      function()
        require("harpoon"):list():add()
      end,
      desc = "Mark file with harpoon",
    },
    {
      "<C-n>",
      function()
        require("harpoon"):list():next()
      end,
      desc = "Go to next harpoon mark",
    },
    {
      "<C-p>",
      function()
        require("harpoon"):list():prev()
      end,
      desc = "Go to previous harpoon mark",
    },
    {
      "<C-e>",
      function()
        require("harpoon").ui:toggle_quick_menu(require("harpoon"):list())
      end,
      desc = "Show harpoon marks",
    },
  },
}
