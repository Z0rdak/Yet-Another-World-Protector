package de.z0rdak.yawp.api.commands;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;

public final class Commands {

    private Commands() {
    }

    public static String buildCommandStr(String... cmdTokens) {
        String baseCmd = "/" + Constants.MOD_ID;
        String cmdStr = String.join(" ", cmdTokens);
        return baseCmd + " " + cmdStr;
    }

    public static String buildRegionBaseCmd(IProtectedRegion region) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                return buildCommandStr(GLOBAL.toString());
            }
            case DIMENSION: {
                return buildCommandStr(DIM.toString(), region.getDim().location().toString());
            }
            case LOCAL: {
                return buildCommandStr(LOCAL.toString(), region.getDim().location().toString(), region.getName());
            }
            default:
                throw new IllegalArgumentException("Unexpected value: " + region.getRegionType());
        }
    }

    public static String buildFlagBaseCmd(IProtectedRegion region, String flag) {
        switch (region.getRegionType()) {
            case GLOBAL: {
                return buildCommandStr(FLAG.toString(), GLOBAL.toString(), flag);
            }
            case DIMENSION: {
                return buildCommandStr(FLAG.toString(), DIM.toString(), region.getDim().location().toString(), flag);
            }
            case LOCAL: {
                return buildCommandStr(FLAG.toString(), LOCAL.toString(), region.getDim().location().toString(), region.getName(), flag);
            }
            default:
                throw new IllegalArgumentException("Unexpected value: " + region.getRegionType());
        }
    }

    public static String buildFlagBaseCmd(IProtectedRegion region, RegionFlag flag) {
        return buildFlagBaseCmd(region, flag.name);
    }

    public static String buildFlagBaseCmd(IProtectedRegion region, IFlag flag) {
        return buildFlagBaseCmd(region, flag.getName());
    }

    public static String buildRegionCmd(IProtectedRegion region, String subCmd) {
        String baseCmd = buildRegionBaseCmd(region);
        return appendSubCommand(baseCmd, subCmd);
    }

    public static String buildFlagCmd(IProtectedRegion region, String flag, String subCmd) {
        String baseCmd = buildFlagBaseCmd(region, flag);
        return appendSubCommand(baseCmd, subCmd);
    }

    public static String buildFlagInfoCmd(IProtectedRegion region, String flag) {
        return buildFlagCmd(region, flag, INFO.toString());
    }

    public static String buildFlagSuggestStateCmd(IProtectedRegion region, String flag) {
        String subCmd = buildSubCmdStr(STATE.toString(), "");
        return buildFlagCmd(region, flag, subCmd);
    }

    public static String buildFlagSetStateCmd(IProtectedRegion region, String flag, FlagState state) {
        String subCmd = buildSubCmdStr(STATE.toString(), state.name);
        return buildFlagCmd(region, flag, subCmd);
    }

    public static String buildFlagOverrideToggleCmd(IProtectedRegion region, String flag) {
        String subCmd = buildSubCmdStr(OVERRIDE.toString());
        return buildFlagCmd(region, flag, subCmd);
    }

    public static String buildFlagMsgCmd(IProtectedRegion region, String flag, String msgSubCmd) {
        String msgCmd = buildFlagCmd(region, flag, MSG.toString());
        return appendSubCommand(msgCmd, msgSubCmd);
    }

    public static String buildFlagMsgSetCmd(IProtectedRegion region, String flag, String msg) {
        String subCmd = buildSubCmdStr(SET.toString(), msg);
        return buildFlagMsgCmd(region, flag, subCmd);
    }

    public static String buildFlagMsgClearCmd(IProtectedRegion region, String flag) {
        String subCmd = buildSubCmdStr(CLEAR.toString());
        return buildFlagMsgCmd(region, flag, subCmd);
    }

    public static String buildFlagMsgMuteToggleCmd(IProtectedRegion region, String flag) {
        String subCmd = buildSubCmdStr(MUTE.toString());
        return buildFlagMsgCmd(region, flag, subCmd);
    }

    public static String buildRegionInfoCmd(IProtectedRegion region) {
        return buildRegionCmd(region, INFO.toString());
    }

    public static String buildRegionStateCmd(IProtectedRegion region) {
        return buildRegionCmd(region, STATE.toString());
    }

    public static String buildRegionStateEnableToggleCmd(IProtectedRegion region) {
        String subCmd = buildSubCmdStr(STATE.toString(), ENABLE.toString());
        return buildRegionCmd(region, subCmd);
    }

    public static String buildRegionStateAlertToggleCmd(IProtectedRegion region) {
        String subCmd = buildSubCmdStr(STATE.toString(), ALERT.toString());
        return buildRegionCmd(region, subCmd);
    }

    private static String buildListCommand(IProtectedRegion region, String listSubCmd) {
        String subCmd = buildRegionCmd(region, LIST.toString());
        return appendSubCommand(subCmd, listSubCmd);
    }

    public static String buildRemoveGroupMemberCommand(IProtectedRegion region, GroupType type, String groupName, String member) {
        String remove = buildSubCmdStr(type.name, groupName, member);
        return buildRemoveCommand(region, remove);
    }

    public static String buildRemoveOfflinePlayerCommand(IProtectedRegion region, String groupName, GroupType type, String player) {
        String remove = buildSubCmdStr(type.name, groupName, BY_NAME.toString(), player);
        return buildRemoveCommand(region, remove);
    }

    public static String buildAddGroupMemberCommand(IProtectedRegion region, GroupType type, String groupName, String member) {
        String addCmd = buildSubCmdStr(type.name, groupName, member);
        return buildAddCommand(region, addCmd);
    }

    private static String buildAddCommand(IProtectedRegion region, String listSubCmd) {
        String subCmd = buildRegionCmd(region, ADD.toString());
        return appendSubCommand(subCmd, listSubCmd);
    }

    private static String buildRemoveCommand(IProtectedRegion region, String listSubCmd) {
        String subCmd = buildRegionCmd(region, REMOVE.toString());
        return appendSubCommand(subCmd, listSubCmd);
    }

    public static String buildSubCmdStr(String... cmdTokens) {
        return String.join(" ", cmdTokens);
    }

    public static String appendSubCommand(String cmd, String... subCommands) {
        return String.join(" ", cmd, String.join(" ", subCommands));
    }

    /**
     * Command for listing responsible flags
     */
    public static String buildListFlagsCommand(IProtectedRegion region) {
        return buildListCommand(region, FLAG.toString());
    }

    /**
     * Command for listing flags only from the provided region
     */
    public static String buildListRegionFlagsCommand(IProtectedRegion region) {
        return buildListCommand(region, REGION_FLAG.toString());
    }

    /**
     * Command for listing responsible flags
     */
    public static String buildAddFlagCommand(IProtectedRegion region, String flag) {
        String subCmd = buildSubCmdStr(FLAG.toString(), flag);
        return buildAddCommand(region, subCmd);
    }

    public static String buildListLocalRegionCommand(ResourceKey<Level> dim) {
        return buildCommandStr(DIM.toString(), dim.location().toString(), LIST.toString(), LOCAL.toString());
    }

    public static String buildListGroupMemberCommand(IProtectedRegion region, String group, GroupType groupType) {
        String subCmd = buildSubCmdStr(GROUP.toString(), group, groupType.name);
        return buildListCommand(region, subCmd);
    }

    public static String buildListGroupCommand(IProtectedRegion region, String group) {
        String subCmd = buildSubCmdStr(GROUP.toString(), group);
        return buildListCommand(region, subCmd);
    }

    public static String buildListChildRegionCommand(IProtectedRegion region) {
        return buildListCommand(region, CHILDREN.toString());
    }
}
