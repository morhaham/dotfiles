return {
  "folke/flash.nvim",
  event = "VeryLazy",
  config = function()
    -- vim.keymap.set("c", "<c-s>", function() require("flash").toggle() end, { noremap = true, silent = true })
    vim.keymap.set({ "n", "x", "o" }, "s", function()
      require("flash").jump()
    end, { noremap = true, silent = true })
    -- vim.keymap.set({ "n", "x", "o" }, "S", function() require("flash").treesitter() end, { noremap = true, silent = true })
    -- vim.keymap.set("o", "r", function() require("flash").remote() end, { noremap = true, silent = true })
    -- vim.keymap.set({"o", "x"}, "R", function() require("flash").treesitter_search() end, { noremap = true, silent = true })
  end,
}
