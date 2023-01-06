package de.z0rdak.yawp.commands;

public enum CommandConstants {
    ACTIVATE("activate"),
    ADD("add"),
    ADD_FLAG("add-flag"),
    ADD_OFFLINE("add-offline"),
    ADD_PLAYER("add-player"),
    ADD_TEAM("add-team"),
    AFFILIATION("affiliation"),
    ALERT("alert"),
    ALL("all"),
    ALLOW("allow"),
    AREA("area"),
    BASE_CMD("wp"),
    BLOCKS("blocks"),
    CHILD("child"),
    CHILDREN("children"),
    CLEAR("clear"),
    CREATE("create"),
    DEACTIVATE("deactivate"),
    DEC("-"),
    DEFAULT_Y("y-default"),
    DELETE("delete"),
    DENY("deny"),
    DIMENSION("dim"),
    ENABLE("enable"),
    INVERT("invert"),
    EXPAND("expand"),
    FLAG("flag"),
    FLAGS("flags"),
    FLAG_TYPE("flag-type"),
    HELP("help"),
    INC("+"),
    INFO("info"),
    LIST("list"),
    BOOL("bool"),
    INT("int"),
    MEMBER("member"),
    NAME("name"),
    MARKER("marker"),
    GIVE("give"),
    OWNER("owner"),
    PARENT("parent"),
    PARENT_REGION("parent-region"),
    PLAYER("player"),
    PLAYERS("players"),
    PRIORITY("priority"),
    REGION("region"),
    REGIONS("regions"),
    REMOVE("remove"),
    REMOVE_ALL("remove-all"),
    REMOVE_FLAG("remove-flag"),
    REMOVE_OFFLINE("remove-offline"),
    REMOVE_PLAYER("remove-player"),
    REMOVE_TEAM("remove-team"),
    RESET("reset"),
    SELECT("select"),
    SET("set"),
    SPATIAL("spatial"),
    STATE("state"),
    TARGET("target"),
    TEAM("team"),
    TEAMS("teams"),
    TELEPORT("tp"),
    TEMPLATE("template"),
    TRIGGER("trigger"),
    TYPE("type"),
    UPDATE("update"),
    VERT("vert"),
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
