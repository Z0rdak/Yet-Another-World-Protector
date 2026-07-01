package de.z0rdak.yawp.api.commands;

import com.sun.jna.WString;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum CommandConstants {
    ADD("add"),
    ASSIGN("assign"),
    REVOKE("revoke"),
    MY("my"),
    REQUEST("request"),
    REQUESTS("requests"),
    RETURN("return"),
    APPROVE("approve"),
    REQUEST_ID("request-id"),
    DENY("deny"),
    GROUP("group"),
    ALERT("alert"),
    ALERT_LOCAL("alert-local"),
    MUTE("mute"),

    AREA("area"),
    POS1("pos1"),
    POS2("pos2"),
    RADIUS("radius"),
    CENTER_POS("center-pos"),

    CHILD("child"),
    SUBREGION("subregion"),
    EXISTING("Existing"),
    CHILDREN("children"),
    PATH("path"),
    ATTACH("attach"),
    DETACH("detach"),
    RECURSIVE("recursive"),
    STYLE("style"),
    INTERSECTING("intersecting"),
    CLEAR("clear"),
    CREATE("create"),
    CREATE_IN("create-in"),
    DEC("-"),
    DELETE("delete"),
    FOR_SURE("-y"),
    DELETE_ALL("delete-all"),
    FOREVER("forever"),
    SERIOUSLY("seriously"),
    DIM("dim"),
    TARGET("target"),
    TARGET_DIM("target-dim"),
    TARGET_REGION("target-region"),
    LOCAL("local"),
    CLAIM("claim"),
    REGION("region"),
    ADMIN("admin"),
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
    CUBOID("Cuboid"),
    SPHERE("Sphere"),
    NUKE_DISPLAY_ENTITIES("nuke-display-entities"),
    MEMBER("member"),
    RENAME("rename"),
    NAME("name"),
    MARKED("Marked"),
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
    TRACK("track"),
    SET("set"),
    STATE("state"),
    MSG("msg"),
    TELEPORT("tp"),
    TP_ANCHOR("tp-anchor"),
    SHOW("show"),
    DISPLAY("display"),
    BLOCK("block"),
    GLOW("glow"),
    LIGHT_LEVEL("light-level"),
    LEVEL("level"),
    HIDE("hide"),
    HIDE_NEAR("hide-near"),
    NEAR("near"),
    HIDE_ALL("hide-all"),
    ALL("all"),
    UNTRACKED("untracked"),
    HIERARCHY("hierarchy"),
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

    public static List<String> getCommandStrings() {
        return Arrays.stream(CommandConstants.values()).map(CommandConstants::toString).collect(Collectors.toList());
    }

    public static boolean isCommandStr(String cmdString) {
        return getCommandStrings().contains(cmdString.toLowerCase());
    }
}
