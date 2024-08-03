return {
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.6",
    name = "telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local builtin = require("telescope.builtin")
      local actions = require("telescope.actions")
      -- local themes = require("telescope.themes")

      vim.keymap.set("n", "<leader>ff", function()
        builtin.find_files({ hidden = true, no_ignore = true })
      end, { desc = "Find files" })
      vim.keymap.set("n", "<leader><leader>", builtin.git_files, { desc = "Find git files" })
      vim.keymap.set("n", "<leader>fg", function()
        builtin.live_grep({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Live grep" })
      vim.keymap.set("n", "<C-x>", function()
        builtin.grep_string({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Grep string under cursor" })
      vim.keymap.set("n", "<C-p>", function()
        builtin.buffers({ sort_mru = true })
      end, { desc = "Find buffers" })
      vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find help tags" })
      vim.keymap.set("n", "<leader>wd", builtin.diagnostics, { desc = "workspace diagnostics" })

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

      require("telescope").load_extension("fzf")
    end,
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
  },
}
