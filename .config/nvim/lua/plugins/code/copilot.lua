return {
  "zbirenbaum/copilot.lua",
  event = { "InsertEnter" },
  enabled = false,
  config = function()
    require("copilot").setup({
      suggestion = {
        auto_trigger = true,
        keymap = {
          accept = "<C-l>",
        },
      },
    })
  end,
}
