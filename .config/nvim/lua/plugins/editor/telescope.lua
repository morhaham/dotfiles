return {
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.6",
    name = "telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local builtin = require("telescope.builtin")
      local actions = require("telescope.actions")
      local themes = require("telescope.themes")

      vim.keymap.set("n", "<leader>ff", function()
        builtin.find_files({ follow = true, hidden = true })
      end, { desc = "Find files" })
      vim.keymap.set("n", "<leader><leader>", builtin.git_files, { desc = "Find git files" })
      vim.keymap.set("n", "<leader>fg", function()
        builtin.live_grep({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Live grep" })
      vim.keymap.set("n", "<C-x>", function()
        builtin.grep_string({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Grep string under cursor" })
      vim.keymap.set("n", "<C-p>", function()
        builtin.buffers(themes.get_dropdown({ sort_mru = true, previewer = false }))
      end, { desc = "Find buffers" })
      vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find help tags" })

      require("telescope").setup({
        pickers = {
          buffers = {
            mappings = {
              i = {
                ["<C-a>"] = actions.toggle_all,
                ["<C-d>"] = actions.delete_buffer,
              },
              n = {
                ["<C-a>"] = actions.toggle_all,
                ["<C-d>"] = actions.delete_buffer,
              },
            },
          },
        },
      })
    end,
  },
}
