package de.z0rdak.yawp.core.group;

public enum GroupType {
    PLAYER("player"),
    TEAM("team");

    public final String name;

    GroupType(String name) {
        this.name = name;
    }

    public static GroupType of(String name) {
        switch (name) {
            case "player":
                return PLAYER;
            case "team":
                return TEAM;

            default:
                return null;
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
