return {
  "stevearc/oil.nvim",
  opts = {
    view_options = {
      show_hidden = true,
    },
    use_default_keymaps = false,
    keymaps = {
      ["g?"] = "actions.show_help",
      ["<CR>"] = "actions.select",
      ["<C-v>"] = "actions.select_vsplit",
      ["<C-s>"] = "actions.select_split",
      ["<C-t>"] = "actions.select_tab",
      ["<C-e>"] = "actions.preview",
      ["<C-c>"] = "actions.close",
      ["<C-r>"] = "actions.refresh",
      ["<C-q>"] = function()
        require("oil.actions").send_to_qflist.callback()
        vim.defer_fn(function()
          vim.cmd("cclose")
          vim.cmd("Trouble quickfix")
        end, 10)
      end,
      ["-"] = "actions.parent",
      ["_"] = "actions.open_cwd",
      ["`"] = "actions.cd",
      ["~"] = "actions.tcd",
      ["gs"] = "actions.change_sort",
      ["gx"] = "actions.open_external",
      ["g."] = "actions.toggle_hidden",
      ["g\\"] = "actions.toggle_trash",
    },
  },
  keys = {
    { "-", "<cmd>Oil<cr>", desc = "Open Oil" },
  },
  -- Optional dependencies
  dependencies = { "nvim-tree/nvim-web-devicons" },
}
