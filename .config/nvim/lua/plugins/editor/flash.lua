return {
  "folke/flash.nvim",
  event = "VeryLazy",
  config = function()
    vim.keymap.set({ "n", "x", "o" }, "s", function()
      require("flash").jump()
    end, { noremap = true, silent = true, desc = "Flash jump" })
  end,
}
