local debug_icons = require("config.icons").debug
return {
  {
    "rcarriga/nvim-dap-ui",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
      "folke/neodev.nvim",
    },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      local dapview = require("dap-view")
      dap.listeners.before.attach.dapui_config = function()
        dapview.open()
      end
      dap.listeners.before.launch.dapui_config = function()
        dapview.open()
      end
      dap.listeners.before.event_terminated.dapui_config = function()
        dapview.close()
      end
      dap.listeners.before.event_exited.dapui_config = function()
        dapview.close()
      end

      -- Define signs for DAP
      vim.fn.sign_define("DapBreakpoint", {
        text = debug_icons.breakpoint,
        texthl = "DapBreakpoint",
        linehl = "DapBreakpointLine",
        numhl = "DapBreakpointNum",
      })
      vim.fn.sign_define("DapBreakpointCondition", {
        text = debug_icons.condition,
        texthl = "DapBreakpointCondition",
        linehl = "DapBreakpointConditionLine",
        numhl = "DapBreakpointConditionNum",
      })
      vim.fn.sign_define("DapBreakpointRejected", {
        text = debug_icons.rejected,
        texthl = "DapBreakpointRejected",
        linehl = "DapBreakpointRejectedLine",
        numhl = "DapBreakpointRejectedNum",
      })
      vim.fn.sign_define("DapLogPoint", {
        text = debug_icons.log,
        texthl = "DapLogPoint",
        linehl = "DapLogPointLine",
        numhl = "DapLogPointNum",
      })
      vim.fn.sign_define("DapStopped", {
        text = debug_icons.stopped,
        texthl = "DapStopped",
        linehl = "DapStoppedLine",
        numhl = "DapStoppedNum",
      })

      require("neodev").setup({
        library = { plugins = { "nvim-dap-ui" }, types = true },
      })
      vim.keymap.set({ "n", "v" }, "<M-;>", dapui.eval, { desc = "Evaluate expression" })

      dapui.setup({
        floating = {
          max_height = 0.8,
          max_width = 0.8,
          border = "rounded",
          mappings = {
            close = { "q", "<Esc>" },
          },
        },
      })
    end,
  },
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      {
        "Joakker/lua-json5",
        build = "./install.sh",
      },
    },
    config = function()
      local dap = require("dap")
      local dap_utils = require("dap.utils")

      vim.keymap.set("n", "<F2>", function()
        dap.continue()
      end, { desc = "Dap Continue" })
      vim.keymap.set("n", "<F3>", function()
        dap.step_over()
      end, { desc = "Dap Step Over" })
      vim.keymap.set("n", "<F4>", function()
        dap.step_into()
      end, { desc = "Dap Step Into" })
      vim.keymap.set("n", "<F5>", function()
        dap.step_out()
      end, { desc = "Dap Step Out" })
      vim.keymap.set("n", "<F6>", function()
        dap.disconnect()
      end, { desc = "Dap Disconnect" })
      vim.keymap.set("n", "<F7>", function()
        dap.terminate()
      end, { desc = "Dap Terminate" })
      vim.keymap.set({ "i", "n" }, "<M-b>", function()
        dap.toggle_breakpoint()
      end, { desc = "Dap toggle breakpoint" })
      vim.keymap.set("n", "<leader>dr", function()
        dap.repl.open()
      end)
      vim.keymap.set("n", "<leader>dl", function()
        dap.run_last()
      end, { desc = "Dap run last config" })
      vim.keymap.set({ "n", "v" }, "<leader>dh", function()
        require("dap.ui.widgets").hover(nil, {
          border = "rounded",
        })
      end, { desc = "Dap hover" })
      vim.keymap.set({ "n", "v" }, "<leader>dp", function()
        require("dap.ui.widgets").preview()
      end)
      vim.keymap.set("n", "<leader>df", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.frames)
      end, { desc = "Dap " })
      vim.keymap.set("n", "<leader>ds", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.scopes)
      end)

      require("dap.ext.vscode").json_decode = require("json5").parse

      local exts = {
        "javascript",
        "typescript",
        "javascriptreact",
        "typescriptreact",
        "vue",
        "svelte",
        "markdown",
        "json",
        "oil",
      }
      -- ╭──────────────────────────────────────────────────────────╮
      -- │ Adapters                                                 │
      -- ╰──────────────────────────────────────────────────────────╯
      dap.adapters["pwa-node"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          args = {
            vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js",
            "${port}",
          },
        },
      }

      dap.adapters["pwa-chrome"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          args = {
            vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js",
            "${port}",
          },
        },
      }

      -- ╭──────────────────────────────────────────────────────────╮
      -- │ Configurations                                           │
      -- ╰──────────────────────────────────────────────────────────╯

      for _, ext in ipairs(exts) do
        dap.configurations[ext] = {
          {
            type = "pwa-chrome",
            request = "launch",
            name = 'Launch Chrome with "localhost"',
            url = function()
              local co = coroutine.running()
              return coroutine.create(function()
                vim.ui.input({ prompt = "Enter URL: ", default = "http://localhost:3000" }, function(url)
                  if url == nil or url == "" then
                    return
                  else
                    coroutine.resume(co, url)
                  end
                end)
              end)
            end,
            webRoot = "${workspaceFolder}",
            protocol = "inspector",
            sourceMaps = true,
            userDataDir = false,
            skipFiles = { "<node_internals>/**", "node_modules/**", "${workspaceFolder}/node_modules/**" },
            resolveSourceMapLocations = {
              "${webRoot}/*",
              "${webRoot}/apps/**/**",
              "${workspaceFolder}/apps/**/**",
              "${webRoot}/packages/**/**",
              "${workspaceFolder}/packages/**/**",
              "${workspaceFolder}/*",
              "!**/node_modules/**",
            },
          },
          {
            name = "Next.js: debug server-side (pwa-node)",
            type = "pwa-node",
            request = "launch",
            port = 9231,
            skipFiles = { "<node_internals>/**", "node_modules/**" },
            cwd = "${workspaceFolder}",
            command = "pnpm start",
          },
          {
            name = "Next.js: debug full stack (pwa-node)",
            type = "pwa-node",
            request = "launch",
            cwd = "${workspaceFolder}",
            program = "${workspaceFolder}/node_modules/next/dist/bin/next",
            runtimeArgs = { "--inspect" },
            skipFiles = { "<node_internals>/**", "node_modules/**" },
            serverReadyAction = {
              action = "debugWithEdge",
              killOnServerStop = true,
              pattern = "- Local=.+(https?=//.+)",
              uriFormat = "%s",
              webRoot = "${workspaceFolder}",
            },
          },
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch Current File (pwa-node)",
            cwd = "${workspaceFolder}",
            args = { "${file}" },
            sourceMaps = true,
            protocol = "inspector",
            runtimeExecutable = "pnpm",
            runtimeArgs = {
              "run-script",
              "dev",
            },
            resolveSourceMapLocations = {
              "${workspaceFolder}/**",
              "!**/node_modules/**",
            },
          },
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch Current File (pwa-node with ts-node)",
            cwd = "${workspaceFolder}",
            args = { "${file}" },
            runtimeExecutable = "${workspaceFolder}/node_modules/.bin/ts-node",
            sourceMaps = true,
            protocol = "inspector",
            skipFiles = { "<node_internals>/**", "node_modules/**" },
            resolveSourceMapLocations = {
              "${workspaceFolder}/**",
              "!**/node_modules/**",
            },
          },
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch Test Current File (pwa-node with jest)",
            cwd = vim.fn.getcwd(),
            runtimeArgs = { "${workspaceFolder}/node_modules/.bin/jest" },
            runtimeExecutable = "node",
            args = { "${file}", "--coverage", "false" },
            rootPath = "${workspaceFolder}",
            sourceMaps = true,
            console = "integratedTerminal",
            internalConsoleOptions = "neverOpen",
            skipFiles = { "<node_internals>/**", "node_modules/**" },
          },
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch Test Current File (pwa-node with vitest)",
            cwd = vim.fn.getcwd(),
            program = "${workspaceFolder}/node_modules/vitest/vitest.mjs",
            args = { "--inspect-brk", "--threads", "false", "run", "${file}" },
            autoAttachChildProcesses = true,
            smartStep = true,
            console = "integratedTerminal",
            skipFiles = { "<node_internals>/**", "node_modules/**" },
          },
          {
            type = "pwa-node",
            request = "launch",
            name = "Launch Test Current File (pwa-node with deno)",
            cwd = vim.fn.getcwd(),
            runtimeArgs = { "test", "--inspect-brk", "--allow-all", "${file}" },
            runtimeExecutable = "deno",
            attachSimplePort = 9229,
          },
          {
            type = "pwa-chrome",
            request = "attach",
            name = "Attach Program (pwa-chrome, select port)",
            program = "${file}",
            cwd = vim.fn.getcwd(),
            sourceMaps = true,
            protocol = "inspector",
            port = function()
              return vim.fn.input("Select port: ", "9222")
            end,
            webRoot = "${workspaceFolder}",
            skipFiles = { "<node_internals>/**", "node_modules/**", "${workspaceFolder}/node_modules/**" },
            resolveSourceMapLocations = {
              "${webRoot}/*",
              "${webRoot}/apps/**/**",
              "${workspaceFolder}/apps/**/**",
              "${webRoot}/packages/**/**",
              "${workspaceFolder}/packages/**/**",
              "${workspaceFolder}/*",
              "!**/node_modules/**",
            },
          },
          {
            type = "pwa-node",
            request = "attach",
            name = "Attach Program (pwa-node, select pid)",
            cwd = vim.fn.getcwd(),
            processId = dap_utils.pick_process,
            skipFiles = { "<node_internals>/**" },
          },
        }
      end
    end,
  },
  {
    "theHamsta/nvim-dap-virtual-text",
    dependencies = { "mfussenegger/nvim-dap" },
    config = function()
      require("nvim-dap-virtual-text").setup()
    end,
  },
  {
    "leoluz/nvim-dap-go",
    dependencies = { "mfussenegger/nvim-dap" },
    config = function()
      require("dap-go").setup({
        dap_config = {
          type = "go",
          name = "Debug",
          request = "launch",
          showLog = false,
          program = "${file}",
          dlvToolPath = vim.fn.exepath("dlv"),
        },
      })
    end,
  },
  {
    "igorlfs/nvim-dap-view",
    -- event -- Not needed as this plugin is a dependency
    keys = {
      {
        "<leader>dv",
        function()
          require("dap-view").toggle()
        end,
        desc = "Toggle nvim-dap-view",
      },
      {
        "S",
        function()
          require("dap-view").jump_to_view("scopes")
        end,
        desc = "Toggle nvim-dap-view scopes",
      },
      {
        "W",
        function()
          require("dap-view").jump_to_view("watches")
        end,
        desc = "Toggle nvim-dap-view watches",
      },
      {
        "E",
        function()
          require("dap-view").jump_to_view("exceptions")
        end,
        desc = "Toggle nvim-dap-view exceptions",
      },
      {
        "B",
        function()
          require("dap-view").jump_to_view("breakpoints")
        end,
        desc = "Toggle nvim-dap-view breakpoints",
      },
      {
        "T",
        function()
          require("dap-view").jump_to_view("threads")
        end,
        desc = "Toggle nvim-dap-view threads",
      },
      {
        "R",
        function()
          require("dap-view").jump_to_view("repl")
        end,
        desc = "Toggle nvim-dap-view repl",
      },
      {
        "C",
        function()
          require("dap-view").jump_to_view("console")
        end,
        desc = "Toggle nvim-dap-view console",
      },
    },
    opts = {
      winbar = {
        default_section = "scopes",
        sections = { "watches", "scopes", "exceptions", "breakpoints", "threads", "repl", "console" },
        headers = {
          breakpoints = debug_icons.breakpoints_tab .. " Breakpoints",
          scopes = debug_icons.scopes_tab .. " Scopes",
          exceptions = debug_icons.exceptions_tab .. " Exceptions",
          watches = debug_icons.watches_tab .. " Watches",
          threads = debug_icons.threads_tab .. "Threads",
          repl = debug_icons.repl_tab .. "REPL",
          console = debug_icons.console_tab .. " Console",
        },
      },
      windows = {
        height = 12,
        terminal = {
          position = "right",
          hide = { "delve" }, -- Hide the terminal for Delve as they don't implement it
          start_hidden = true,
        },
      },
    },
  },
}
