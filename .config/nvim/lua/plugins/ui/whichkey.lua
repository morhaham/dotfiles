return {
  "folke/which-key.nvim",
  lazy = false,
  init = function()
    vim.keymap.set("n", "<leader>c", "", { desc = "Code" })
    vim.keymap.set("n", "<leader>f", "", { desc = "Find" })
    vim.keymap.set("n", "<leader>b", "", { desc = "Buffer" })
    vim.keymap.set("n", "<leader>d", "", { desc = "Debug" })
    vim.keymap.set("n", "<leader>g", "", { desc = "Git" })
    vim.keymap.set("n", "<leader>p", "", { desc = "Yanky" })
    vim.keymap.set("n", "<leader>t", "", { desc = "Test" })
    vim.keymap.set("n", "<leader>w", "", { desc = "Workspace" })
    vim.keymap.set("n", "<leader>x", "", { desc = "Diagnostics" })
  end,
  opts = {
    win = {
      border = "rounded",
    },
  },
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
}
