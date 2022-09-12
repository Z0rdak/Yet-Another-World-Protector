package de.z0rdak.yawp.commands;

public enum CommandConstants {
    BASE_CMD("wp"),
    REGION("region"),
    AREA("area"),
    REGIONS("regions"),
    DIMENSION("dim"),
    SELECT("select"),
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
    PARENT("parent"),
    SET("set"),
    CLEAR("clear"),
    PARENT_REGION("parent-region"),
    CHILD_REGION("child-region"),
    RESET("reset"),
    REMOVE_OFFLINE("remove-offline"),
    REMOVE_ALL("remove-all"),
    ALL("all"),
    INFO("info"),
    SPATIAL("spatial"),
    IN("in"),
    STATE("state"),
    NAME("name"),
    EXPAND("expand"),
    VERT("vert"),
    DEFAULT_Y("y-default"),
    LIST("list"),
    CHILDREN("children"),
    CREATE("create"),
    DELETE("delete"),
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
    AFFILIATION("affiliation"),
    TEAM("team"),
    TEAMS("teams"),
    PLAYERS("players"),
    OWNER("owner"),
    MEMBER("member"),
    Y1("Y1"),
    Y2("Y2"),

    TEMPLATE("template"),
    TRIGGER("trigger");

    private final String cmdString;

    CommandConstants(final String cmdString) {
        this.cmdString = cmdString;
    }

    @Override
    public String toString() {
        return cmdString;
    }
}
