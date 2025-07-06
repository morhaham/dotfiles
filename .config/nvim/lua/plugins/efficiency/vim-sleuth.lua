return {
  "tpope/vim-sleuth",
  event = "BufReadPre",
  config = function()
    -- No additional configuration needed, vim-sleuth automatically detects
    -- indentation settings based on the file content.
  end,
  lazy = true,
  enabled = true,
}
