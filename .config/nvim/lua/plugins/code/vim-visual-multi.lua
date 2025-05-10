return {
  "mg979/vim-visual-multi",
  branch = "master",
  init = function()
    -- vim.g["VM_default_mappings"] = 0
    vim.g.VM_maps = {
      ["Find Under"] = "<M-n>",
      ["Find Subword Under"] = "<M-n>",
      ["Add Cursor Down"] = "<M-Down>",
      ["Add Cursor Up"] = "<M-Up>",
      ["Add Cursor At Pos"] = "<C-m>",
    }
  end,
}
