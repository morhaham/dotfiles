local have_make = vim.fn.executable("make") == 1
local have_cmake = vim.fn.executable("cmake") == 1

return {
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.8",
    name = "telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-dap.nvim",
      "gbprod/yanky.nvim",
      {
        "nvim-telescope/telescope-live-grep-args.nvim",
        version = "^1.0.0",
      },
      "nvim-telescope/telescope-ui-select.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
      "aaronhallaert/advanced-git-search.nvim",
      "nvim-telescope/telescope-z.nvim",
    },
    config = function()
      local builtin = require("telescope.builtin")
      local actions = require("telescope.actions")
      local open_with_trouble = require("trouble.sources.telescope").open
      local add_to_trouble = require("trouble.sources.telescope").add
      local telescope = require("telescope")
      local lga_actions = require("telescope-live-grep-args.actions")
      local lga_shortcuts = require("telescope-live-grep-args.shortcuts")

      vim.keymap.set("n", "<leader>ff", function()
        builtin.find_files({ hidden = true, no_ignore = true })
      end, { desc = "Find files" })

      vim.keymap.set("n", "<leader><leader>", function()
        builtin.git_files()
      end, { desc = "Find git files" })

      -- Using <C-_> since <C-/> is not supported in some terminals
      vim.keymap.set("n", "<C-_>", function()
        telescope.extensions.live_grep_args.live_grep_args({
          additional_args = { "--follow", "--hidden", "--glob=!.git/" },
        })
      end, { desc = "Live grep(args)" })

      vim.keymap.set("n", "<C-x>", function()
        lga_shortcuts.grep_word_under_cursor({ additional_args = { "--follow", "--hidden", "--glob=!.git/" } })
      end, { desc = "Grep string under cursor" })

      vim.keymap.set("n", "<leader>fb", function()
        builtin.buffers({ sort_mru = true })
      end, { desc = "Find buffers" })

      -- resume the last telescope picker
      vim.keymap.set("n", "<leader>fr", builtin.resume, { desc = "Resume last picker" })

      vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find help tags" })
      vim.keymap.set("n", "<leader>wd", builtin.diagnostics, { desc = "Workspace diagnostics" })

      -- Git related
      vim.keymap.set("n", "<leader>fc", builtin.git_bcommits, { desc = "File commits" })
      vim.keymap.set("n", "<leader>ga", "<CMD>AdvancedGitSearch<CR>", { desc = "Git advanced(Telescope)" })

      -- File browser
      vim.keymap.set("n", "-", function()
        telescope.extensions.file_browser.file_browser()
      end)

      vim.keymap.set("n", "<leader>pp", function()
        telescope.extensions.yank_history.yank_history()
      end)

      -- Copy the current path to clipboard
      local function copy_path()
        local entry = require("telescope.actions.state").get_selected_entry()
        local cb_opts = vim.opt.clipboard:get()
        if vim.tbl_contains(cb_opts, "unnamed") then
          vim.fn.setreg("*", entry.path)
        end
        if vim.tbl_contains(cb_opts, "unnamedplus") then
          vim.fn.setreg("+", entry.path)
        end
        vim.fn.setreg("", entry.path)
      end

      telescope.setup({
        defaults = {
          path_display = { "truncate" },
          mappings = {
            i = { ["<C-q>"] = open_with_trouble, ["<M-q>"] = add_to_trouble },
            n = { ["<C-q>"] = open_with_trouble, ["<M-q>"] = add_to_trouble },
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
        extensions = {
          file_browser = {
            path = "%:p:h",
            prompt_path = true,
            hijack_netrw = true,
            mappings = {
              n = {
                ["-"] = telescope.extensions.file_browser.actions.goto_parent_dir,
                ["<C-y>"] = copy_path,
              },
              i = {
                ["<C-y>"] = copy_path,
              },
            },
          },
          live_grep_args = {
            auto_quoting = true, -- enable/disable auto-quoting
            -- define mappings, e.g.
            mappings = { -- extend mappings
              i = {
                ["<C-k>"] = lga_actions.quote_prompt(),
                ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
                -- freeze the current list and start a fuzzy search in the frozen list
                -- ["<C-r>"] = lga_actions.to_fuzzy_refine,
              },
            },
            -- ... also accepts theme settings, for example:
            -- theme = "dropdown", -- use dropdown theme
            -- theme = { }, -- use own theme spec
            -- layout_config = { mirror=true }, -- mirror preview pane
          },
        },
      })

      telescope.load_extension("file_browser")
      telescope.load_extension("fzf")
      telescope.load_extension("dap")
      telescope.load_extension("noice")
      telescope.load_extension("ui-select")
      telescope.load_extension("advanced_git_search")
      telescope.load_extension("z")
      telescope.load_extension("yank_history")
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
      manual_mode = true,
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
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
  },
  {
    "aaronhallaert/advanced-git-search.nvim",
    cmd = { "AdvancedGitSearch" },
    dependencies = {
      "tpope/vim-fugitive",
      "tpope/vim-rhubarb",
    },
  },
}
