package de.z0rdak.regionshield.commands;

public enum CommandConstants {
    BASE_CMD("rs"),
    REGION("region"),
    REGIONS("regions"),
    DIMENSION("dimension"),
    FLAG("flag"),
    HELP("help"),
    ADD("add"),
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
    OWNER("owner"),
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
