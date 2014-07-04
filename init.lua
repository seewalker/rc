hydra.alert("Hydra sample config loaded", 1.5)

hotkey.bind({"cmd", "ctrl", "alt"}, "R", repl.open)

function checkforupdates () 
    updates.check()
    settings.set('lastcheckedupdates', os.time())
end

menu.show(function()
    local updatetitles = {[true] = "Install Update", [false] = "Check for update..."}
    local updatefns = {[true] = updates.install, [false] = checkforupdates}
    local hasupdate = (updates.newversion ~= nil)

    return {
        {title = "Reload Config", fn = hydra.reload},
        {title = "-"},
        {title = "About", fn = hydra.showabout},
        {title = updatetitles[hasupdate], fn = updatefns[hasupdate]},
        {title = "Quit Hydra", fn = os.exit}
    }
end)

hotkey.new( {"cmd", "ctrl", "alt"}, "J", function() 
    local win = window.focusedwindow()
    local frame = win:frame()
    frame.x = frame.x + 10
    frame.h = frame.h - 10
    win:setframe(frame)
end):enable()

local function showupdate()
    os.execute('open https://github.com/sdegutis/Hydra/releases')
end

function updates.available(available)
    if available then
        notify.show("Hydra update available", "", "Click here to see the changelog and maybe even install it", "showupdate")
    else
        hydra.alert("No update available.")
    end
end

timer.new(timer.weeks(1), checkforupdates):start()
notify.register("showupdate", showupdate)

local lastcheckedupdates = settings.get('lastcheckedupdates')
if lastcheckedupdates == nil or lastcheckedupdates <= os.time() - timer.days(7) then
    checkforupdates()
end
