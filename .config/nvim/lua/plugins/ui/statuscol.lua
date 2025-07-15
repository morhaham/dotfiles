-- Control the status col
return {
  "luukvbaal/statuscol.nvim",
  config = function()
    local builtin = require("statuscol.builtin")

    require("statuscol").setup({
      relculright = true,
      ft_ignore = {
        "NvimTree",
        "dashboard",
        "nvcheatsheet",
        "dapui_watches",
        "dap-repl",
        "dapui_console",
        "dapui_stacks",
        "dapui_breakpoints",
        "dapui_scopes",
        "help",
        "vim",
        "alpha",
        "dashboard",
        "neo-tree",
        "Trouble",
        "noice",
        "lazy",
        "toggleterm",
      },
      segments = {
        -- Simulate the sign column while not showing the gitsigns
        {
          sign = {
            namespace = { "gitsigns" },
            colwidth = 2,
            wrap = true,
            foldclosed = true,
          },
          condition = {
            function(args)
              return vim.wo[args.win].number or vim.b[args.buf].gitsigns_status
            end,
          },
          click = "v:lua.ScSa",
        },
        {
          sign = {
            name = { ".*" },
            text = { ".*" },
            colwidth = 1,
          },
          click = "v:lua.ScSa",
        },
        -- -- Show the fold column with custom icons
        -- {
        --   text = {
        --     function(args)
        --       args.fold.close = ""
        --       args.fold.open = ""
        --       args.fold.sep = " "
        --       return builtin.foldfunc(args)
        --     end,
        --   },
        --   click = "v:lua.ScFa",
        -- },
        -- Simulate the line number column without the right padding
        {
          text = { builtin.lnumfunc, " " },
          click = "v:lua.ScLa",
        },
      },
    })
  end,
}
