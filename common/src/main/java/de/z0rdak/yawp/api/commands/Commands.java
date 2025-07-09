package de.z0rdak.yawp.api.commands;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.DisplayType;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.GroupType;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.commandBlockPosStr;

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


    public static String buildDisplayCommand(IMarkableRegion region) {
        return buildRegionCmd(region, DISPLAY.toString());
    }

    public static String buildShowCommand(IMarkableRegion region) {
        return buildRegionCmd(region, SHOW.toString());
    }

    public static String buildShowSubCommand(IMarkableRegion region, String subCmd) {
        String baseCmd = buildShowCommand(region);
        return appendSubCommand(baseCmd, subCmd);
    }

    public static String buildHideCommand(IMarkableRegion region) {
        return buildRegionCmd(region, HIDE.toString());
    }

    public static String buildHideSubCommand(IMarkableRegion region, String subCmd) {
        String baseCmd = buildHideCommand(region);
        return appendSubCommand(baseCmd, subCmd);
    }

    public static String buildDisplaySubCommand(IMarkableRegion region, String subCmd) {
        String baseCmd = buildDisplayCommand(region);
        return appendSubCommand(baseCmd, subCmd);
    }

    public static String buildSetDisplayBlockCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(BLOCK.toString(), "");
        return buildDisplaySubCommand(region, subCmd);
    }

    public static String buildSetDisplayBlockCommand(IMarkableRegion region, ResourceLocation block) {
        String subCmd = buildSubCmdStr(BLOCK.toString(), block.toString());
        return buildDisplaySubCommand(region, subCmd);
    }

    public static String buildSetDisplayGlowCommand(IMarkableRegion region, boolean on) {
        String subCmd = buildSubCmdStr(GLOW.toString(), Boolean.toString(on));
        return buildDisplaySubCommand(region, subCmd);
    }

    public static String buildSetDisplayLightLevelCommand(IMarkableRegion region, int lightLevel) {
        String subCmd = buildSubCmdStr(LIGHT_LEVEL.toString(), String.valueOf(lightLevel));
        return buildDisplaySubCommand(region, subCmd);
    }

    public static String buildSetDisplayLightLevelCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(LIGHT_LEVEL.toString(), "");
        return buildDisplaySubCommand(region, subCmd);
    }

    public static String buildVisualizationHideCommand(IMarkableRegion region, DisplayType displayType) {
        String subCmd = buildSubCmdStr(LOCAL.toString(), displayType.name);
        return buildHideSubCommand(region, subCmd);
    }

    public static String buildVisualizationShowCommand(IMarkableRegion region, DisplayType displayType) {
        String subCmd = buildSubCmdStr(LOCAL.toString(), displayType.name);
        return buildShowSubCommand(region, subCmd);
    }

    public static String buildAdvancedVisualizationShowCommand(IMarkableRegion region, DisplayType displayType, ResourceLocation block, boolean glow, int lightLevel) {
        String subCmd = buildSubCmdStr(LOCAL.toString(), displayType.name, block.toString(), String.valueOf(glow), String.valueOf(lightLevel));
        return buildShowSubCommand(region, subCmd);
    }

    public static String buildVisualizationHideHierarchyCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(HIERARCHY.toString());
        return buildHideSubCommand(region, subCmd);
    }

    public static String buildVisualizationShowHierarchyCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(HIERARCHY.toString());
        return buildShowSubCommand(region, subCmd);
    }

    public static String buildVisualizationHideIntersectingCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(INTERSECTING.toString());
        return buildHideSubCommand(region, subCmd);
    }

    public static String buildVisualizationShowIntersectingCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(INTERSECTING.toString());
        return buildShowSubCommand(region, subCmd);
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

    public static String buildTeleportAnchorListCommand(IProtectedRegion region) {
        return buildRegionCmd(region, TP_ANCHOR.toString());
    }

    public static String buildTeleportAnchorSubCommand(IMarkableRegion region, String subCmd) {
        String baseCmd = buildTeleportAnchorListCommand(region);
        return appendSubCommand(baseCmd, subCmd);
    }

    public static String buildTeleportTpAnchorCommand(IMarkableRegion region, String name) {
        String subCmd = buildSubCmdStr(TELEPORT.toString(), name);
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildShowTpAnchorCommand(IMarkableRegion region, String name) {
        String subCmd = buildSubCmdStr(HIDE.toString(), name);
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildHideTpAnchorCommand(IMarkableRegion region, String name) {
        String subCmd = buildSubCmdStr(SHOW.toString(), name);
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildSuggestRenameTpAnchorCommand(IMarkableRegion region, String name) {
        String subCmd = buildSubCmdStr(RENAME.toString(), name, "");
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildRenameTpAnchorCommand(IMarkableRegion region, String name, String newName) {
        String subCmd = buildSubCmdStr(RENAME.toString(), name, newName);
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildSuggestUpdateTpAnchorCommand(IMarkableRegion region, String name) {
        String subCmd = buildSubCmdStr(SET.toString(), name, "");
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildUpdateTpAnchorCommand(IMarkableRegion region, String name, BlockPos pos) {
        String subCmd = buildSubCmdStr(SET.toString(), name, commandBlockPosStr(pos));
        return buildTeleportAnchorSubCommand(region, subCmd);
    }

    public static String buildAddTeleportAnchorCommand(IMarkableRegion region, String name, BlockPos pos) {
        String subCmd = buildSubCmdStr(TP_ANCHOR.toString(), name, commandBlockPosStr(pos));
        return buildAddCommand(region, subCmd);
    }

    public static String buildSuggestAddTeleportAnchorCommand(IMarkableRegion region) {
        String subCmd = buildSubCmdStr(TP_ANCHOR.toString(), "");
        return buildAddCommand(region, subCmd);
    }

    public static String buildRemoveTeleportAnchorCommand(IMarkableRegion region, String name) {
        String subCmd = buildSubCmdStr(TP_ANCHOR.toString(), name);
        return buildRemoveCommand(region, subCmd);
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
