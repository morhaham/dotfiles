local have_make = vim.fn.executable("make") == 1
local have_cmake = vim.fn.executable("cmake") == 1

return {
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.6",
    name = "telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-dap.nvim",
      {
        "nvim-telescope/telescope-live-grep-args.nvim",
        version = "^1.0.0",
      },
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      local builtin = require("telescope.builtin")
      local actions = require("telescope.actions")
      local open_with_trouble = require("trouble.sources.telescope").open
      -- Use this to add more results without clearing the trouble list
      local add_to_trouble = require("trouble.sources.telescope").add
      -- local themes = require("telescope.themes")

      vim.keymap.set("n", "<leader>ff", function()
        builtin.find_files({ hidden = true, no_ignore = true })
      end, { desc = "Find files" })

      vim.keymap.set("n", "<leader><leader>", builtin.git_files, { desc = "Find git files" })

      vim.keymap.set("n", "<leader>fg", function()
        require("telescope").extensions.live_grep_args.live_grep_args({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Live grep(args)" })

      vim.keymap.set("n", "<C-x>", function()
        builtin.grep_string({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Grep string under cursor" })

      vim.keymap.set("n", "<leader>fb", function()
        builtin.buffers({ sort_mru = true })
      end, { desc = "Find buffers" })

      vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find help tags" })
      vim.keymap.set("n", "<leader>wd", builtin.diagnostics, { desc = "Workspace diagnostics" })

      -- Git related
      vim.keymap.set("n", "<leader>fc", builtin.git_bcommits, { desc = "File commits" })

      require("telescope").setup({
        defaults = {
          mappings = {
            i = { ["<C-q>"] = open_with_trouble },
            n = { ["<C-q>"] = open_with_trouble },
          },
        },
        pickers = {
          find_files = {
            hidden = true,
            no_ignore = true,
          },
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
      require("telescope").load_extension("dap")
      require("telescope").load_extension("ui-select")
    end,
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = have_make and "make"
      or "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    enabled = have_make or have_cmake,
  },
  {
    "ahmedkhalf/project.nvim",
    opts = {
      detection_methods = { "lsp", "pattern" },
      patterns = { ".git", "Cargo.toml", ".hg", ".bzr", ".svn", "Makefile" },
      show_hidden = true,
    },
    event = "VeryLazy",
    config = function(_, opts)
      require("project_nvim").setup(opts)
      require("telescope").load_extension("projects")
      vim.keymap.set("n", "<leader>fp", ":Telescope projects <CR>")
    end,
  },
}
