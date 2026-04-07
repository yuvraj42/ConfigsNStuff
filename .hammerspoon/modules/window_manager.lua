local log = hs.logger.new("window_groups", "debug")

local communication = {"Slack", "Outlook", "Teams"}
local terminals     = {"Ghostty", "Terminal", "iTerm"}
local browsers      = {"Chrome", "Firefox", "Safari"}
local editors       = {"Cursor", "PyCharm", "Code", "Emacs"}

local appGroups = {
  [1] = communication,
  [2] = terminals,
  [3] = browsers,
  [4] = editors,
}

local lastActivated = {}

local function isGhostWindow(w)
  local ok, title = pcall(function() return w:title() end)
  if not ok then return true end
  return title:match("^%d+ Reminder")
end

-- Match last word of app name or exact name, case-insensitive.
-- "Teams" → "Microsoft Teams", "Chrome" → "Google Chrome"
local function appMatchesGroup(appName, group)
  local lower = appName:lower()
  local lastWord = lower:match("(%S+)%s*$") or lower
  for _, pattern in ipairs(group) do
    local p = pattern:lower()
    if lower == p or lastWord == p then return true end
  end
  return false
end

local trackedIds = {}
local windowObjs = {}
for i = 1, 4 do trackedIds[i] = {} end

local function scanApp(app)
  for groupKey, group in pairs(appGroups) do
    if appMatchesGroup(app:name(), group) then
      for _, w in ipairs(app:allWindows()) do
        local ok, std = pcall(function() return w:isStandard() end)
        if ok and std and not isGhostWindow(w) then
          local wid = w:id()
          if not trackedIds[groupKey][wid] then
            trackedIds[groupKey][wid] = true
            windowObjs[wid] = w
          end
        end
      end
    end
  end
end

for _, app in ipairs(hs.application.runningApplications()) do scanApp(app) end

local appWatcher = hs.application.watcher.new(function(appName, event, app)
  if not app then return end
  if event == hs.application.watcher.activated then
    for _, group in pairs(appGroups) do
      if appMatchesGroup(appName, group) then scanApp(app); break end
    end
  elseif event == hs.application.watcher.launched then
    -- delay scan so the app's windows have time to open
    hs.timer.doAfter(1, function()
      local a = hs.application.get(appName)
      if a then
        for _, group in pairs(appGroups) do
          if appMatchesGroup(appName, group) then scanApp(a); break end
        end
      end
    end)
  end
end)
appWatcher:start()

local function getGroupWindows(groupKey)
  local windows, stale = {}, {}
  for wid in pairs(trackedIds[groupKey]) do
    local w = windowObjs[wid]
    local ok, app = pcall(function() return w:application() end)
    if ok and app then table.insert(windows, w)
    else table.insert(stale, wid) end
  end
  for _, wid in ipairs(stale) do
    trackedIds[groupKey][wid] = nil; windowObjs[wid] = nil
  end
  table.sort(windows, function(a, b) return a:id() < b:id() end)
  return windows
end

local function cycleAppGroup(groupKey, group)
  -- Scan on every press to catch apps launched after Hammerspoon loaded
  for _, app in ipairs(hs.application.runningApplications()) do
    if appMatchesGroup(app:name(), group) then scanApp(app) end
  end
  local windows = getGroupWindows(groupKey)

  if #windows == 0 then
    for _, app in ipairs(hs.application.runningApplications()) do
      if appMatchesGroup(app:name(), group) then
        local aref = app
        app:activate()
        hs.timer.doAfter(0.5, function()
          scanApp(aref)
          local wins = getGroupWindows(groupKey)
          if #wins > 0 then
            wins[1]:focus(); lastActivated[groupKey] = wins[1]:id()
          end
        end)
        return
      end
    end
    return
  end

  local focused = hs.window.focusedWindow()
  local currentIdx = nil
  if focused then
    for i, w in ipairs(windows) do
      if w:id() == focused:id() then currentIdx = i; break end
    end
  end
  if not currentIdx then
    local lastId = lastActivated[groupKey]
    if lastId then
      for i, w in ipairs(windows) do
        if w:id() == lastId then currentIdx = i; break end
      end
    end
  end

  local nextIdx = currentIdx and (currentIdx % #windows) + 1 or 1
  local nextWin = windows[nextIdx]
  local title = nextWin:title()
  log.d("Switching to: " .. (title ~= "" and title or nextWin:application():name()))
  lastActivated[groupKey] = nextWin:id()
  nextWin:focus()
end

for i, group in pairs(appGroups) do
  local g, key = group, i
  hs.hotkey.bind({"ctrl", "alt"}, tostring(i), function()
    log.d("Hotkey triggered for group " .. key)
    cycleAppGroup(key, g)
  end)
end
