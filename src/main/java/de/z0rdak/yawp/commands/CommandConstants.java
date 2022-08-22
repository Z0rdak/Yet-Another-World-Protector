package de.z0rdak.yawp.commands;

public enum CommandConstants {
    BASE_CMD("wp"),
    REGION("region"),
    REGIONS("regions"),
    DIMENSION("dim"),
    DIM_FLAG("dim-flag"),
    FLAG("flag"),
    FLAGS("flags"),
    HELP("help"),
    ADD("add"),
    ADD_FLAG("add-flag"),
    ADD_TEAM("add-team"),
    ADD_PLAYER("add-player"),
    REMOVE_FLAG("remove-flag"),
    REMOVE_TEAM("remove-team"),
    REMOVE_PLAYER("remove-player"),
    ADD_OFFLINE("add-offline"),
    REMOVE("remove"),
    RESET("reset"),
    REMOVE_OFFLINE("remove-offline"),
    REMOVE_ALL("remove-all"),
    ALL("all"),
    INFO("info"),
    NAME("name"),
    EXPAND("expand"),
    VERT("vert"),
    DEFAULT_Y("y-default"),
    LIST("list"),
    CREATE("create"),
    UPDATE("update"),
    TELEPORT("tp"),
    ACTIVATE("activate"),
    DEACTIVATE("deactivate"),
    ALERT("alert"),
    ENABLE("enable"),
    PRIORITY("priority"),
    FLAG_TYPE("flag-type"),
    TYPE("type"),
    ALLOW("allow"),
    DENY("deny"),
    PLAYER("player"),
    TEAM("team"),
    TEAMS("teams"),
    PLAYERS("players"),
    OWNER("owner"),
    MEMBER("member"),
    Y1("Y1"),
    Y2("Y2");

    private final String cmdString;

    CommandConstants(final String cmdString) {
        this.cmdString = cmdString;
    }

    @Override
    public String toString() {
        return cmdString;
    }
}
