local git_signs_icons = require("config.icons").git.signs
return {
  "lewis6991/gitsigns.nvim",
  config = function()
    require("gitsigns").setup({
      -- signs = {
      --   add = { text = git_signs_icons.add },
      --   change = { text = git_signs_icons.change },
      --   delete = { text = git_signs_icons.delete },
      --   topdelete = { text = git_signs_icons.topdelete },
      --   changedelete = { text = git_signs_icons.changedelete },
      --   untracked = { text = git_signs_icons.untracked },
      -- },
      -- signs_staged = {
      --   add = { text = git_signs_icons.add },
      --   change = { text = git_signs_icons.change },
      --   delete = { text = git_signs_icons.delete },
      --   topdelete = { text = git_signs_icons.topdelete },
      --   changedelete = { text = git_signs_icons.changedelete },
      --   untracked = { text = git_signs_icons.untracked },
      -- },
      current_line_blame = true,
    })
  end,
}
