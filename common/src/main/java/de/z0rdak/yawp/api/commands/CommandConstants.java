package de.z0rdak.yawp.api.commands;

public enum CommandConstants {
    ADD("add"),
    GROUP("group"),
    ALERT("alert"),
    ALERT_LOCAL("alert-local"),
    MUTE("mute"),

    AREA("area"),
    POS1("pos1"),
    POS2("pos2"),
    RADIUS_POS("radius-pos"),
    RADIUS("radius"),
    CENTER_POS("center-pos"),

    CHILD("child"),
    CHILDREN("children"),
    CLEAR("clear"),
    CREATE("create"),
    DEC("-"),
    DELETE("delete"),
    FOR_SURE("-y"),
    DELETE_ALL("delete-all"),
    FOREVER("forever"),
    SERIOUSLY("seriously"),
    DIM("dim"),
    TARGET_DIM("target-dim"),
    TARGET_REGION("target-region"),
    LOCAL("local"),
    TO_LOCAL("to-local"),
    TO_DIM("to-dim"),
    GLOBAL("global"),
    ENABLE("enable"),
    ENABLE_LOCAL("enable-local"),
    OVERRIDE("override"),
    EXPAND("expand"),
    FLAG("flag"),
    REGION_FLAG("region-flag"),
    FLAGS("flags"),
    ALL_FLAGS("all-flags"),
    HELP("help"),
    INC("+"),
    INFO("info"),
    COPY("copy"),
    LIST("list"),
    MEMBER("member"),
    RENAME("rename"),
    NAME("name"),
    MARKER("marker"),
    GIVE("give"),
    OWNER("owner"),
    PARENT("parent"),
    PLAYER("player"),
    PLAYERS("players"),
    PLAYER_NAMES("player-names"),
    BY_NAME("by-name"),
    PLAYER_UUID("player-uuid"),
    BY_UUID("by-uuid"),
    PRIORITY("priority"),
    PAGE("page"),
    REGIONS("regions"),
    REMOVE("remove"),
    RESET("reset"),
    SELECT("select"),
    DESELECT("deselect"),
    SET("set"),
    STATE("state"),
    MSG("msg"),
    TARGET("target"),
    TEAM("team"),
    TEAMS("teams"),
    TELEPORT("tp"),
    TYPE("type"),
    EXPANSION("expansion"),
    Y_MIN("y-min"),
    Y_MAX("y-max");

    private final String cmdString;

    CommandConstants(final String cmdString) {
        this.cmdString = cmdString;
    }

    @Override
    public String toString() {
        return cmdString;
    }
}
