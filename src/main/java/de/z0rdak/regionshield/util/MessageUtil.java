package de.z0rdak.regionshield.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.commands.CommandConstants;
import de.z0rdak.regionshield.core.region.AbstractMarkableRegion;
import de.z0rdak.regionshield.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.*;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.util.text.event.HoverEvent;
import net.minecraft.world.World;

import static de.z0rdak.regionshield.config.ServerRegionConfigBuilder.REGION_DEFAULT_PRIORITY_INC;

public class MessageUtil {

    private MessageUtil() {
    }

    public static void sendCmdFeedback(CommandSource src, IFormattableTextComponent text) {
        try {
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                MessageUtil.sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            RegionShield.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(CommandSource src, String langKey) {
        try {
            TranslationTextComponent text = new TranslationTextComponent(langKey);
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                MessageUtil.sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            RegionShield.LOGGER.error(e);
        }
    }

    public static void sendMessage(PlayerEntity player, ITextComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(new TranslationTextComponent(translationKey), player.getUUID());
    }

    private static String format(double value) {
        return String.format("%.2f", value);
    }

    public static void sendRegionInfoCommand(String regionName, PlayerEntity player) {
        RegionDataManager.get().getRegion(regionName).ifPresent(region -> {
            BlockPos target = region.getTpTarget();
            String regionInfoCommand = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.REGION + " " + CommandConstants.INFO + " " + regionName;
            String regionTeleportCommand = "/tp @s " + target.getX() + " " + target.getY() + " " + target.getZ();

            IFormattableTextComponent regionMsg = new StringTextComponent("Region '")
                    .append(buildExecuteCmdComponent(regionName, regionInfoCommand,
                            TextFormatting.GREEN, "chat.link.hover.region.info", ClickEvent.Action.RUN_COMMAND))
                    .append(new StringTextComponent("': ").setStyle(Style.EMPTY.withColor(Color.fromLegacyFormat(TextFormatting.RESET))))
                    .append(buildExecuteCmdComponent(target.getX() + ", " + target.getY() + ", " + target.getZ(), regionTeleportCommand,
                            TextFormatting.GREEN, "chat.link.hover.region.tp" , ClickEvent.Action.RUN_COMMAND));
            sendMessage(player, regionMsg);
        });
    }

    public static IFormattableTextComponent buildExecuteCmdComponent(String linkText, String command, TextFormatting color, String hoverText, ClickEvent.Action eventAction){
        return TextComponentUtils.wrapInSquareBrackets(new StringTextComponent(linkText))
                .setStyle(Style.EMPTY.withColor(Color.fromLegacyFormat(color))
                        .withClickEvent(new ClickEvent(eventAction, command))
                        .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent(hoverText))));
    }

    public static IFormattableTextComponent buildDimensionTeleportLink(AbstractMarkableRegion region) {
        String cmd = buildTeleportCmd("@s", region.getTpTarget());
        String cmdLinkText = buildTeleportLinkText(region.getDim(), region.getTpTarget());
        String executeCmdStr = buildExecuteCommandString(region.getDim(), cmd);
        return buildExecuteCmdComponent(cmdLinkText, executeCmdStr, TextFormatting.GREEN, "chat.link.hover.region.tp", ClickEvent.Action.RUN_COMMAND);
    }

    public static String buildTeleportCmd(String tpSource, BlockPos target){
        return "tp " + tpSource + " " + target.getX() + " " + target.getY() + " " + target.getZ();
    }

    public static String buildTeleportLinkText(RegistryKey<World> dim, BlockPos target){
        return dim + "@ [" + target.getX() + ", " + target.getY() + ", " + target.getZ() + "]";
    }

    public static String buildExecuteCommandString(RegistryKey<World> dim, String command){
        return "/execute in " + dim.location() + " run " + command;
    }

    public static void promptRegionFlags(PlayerEntity player, String regionName) {
        if(RegionDataManager.get().containsRegion(regionName)) {
            RegionDataManager.get().getRegion(regionName).ifPresent(region -> {
                // TODO: lang-key
                sendMessage(player, new TranslationTextComponent(TextFormatting.BOLD + "== Flags in Region '" + regionName + "' =="));
                if (region.getFlags().isEmpty()) {
                    sendMessage(player, new TranslationTextComponent("message.region.info.noflags"));
                    return;
                }
                region.getFlags().forEach(flag -> {
                    sendMessage(player, MessageUtil.buildRemoveFlagLink(flag.getFlagName(), regionName));
                });
                sendMessage(player, new StringTextComponent(""));
            });
        } else {
            sendMessage(player, new TranslationTextComponent("message.region.unknown", regionName));
        }
    }

    public static IFormattableTextComponent buildRemoveFlagLink(String flag, String region) {
        String removeFlagCommand = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.FLAG + " " + CommandConstants.REMOVE + " " + region + " " + flag;
        return new StringTextComponent(" - ")
                // TODO: lang-key
                .append(buildExecuteCmdComponent("x", removeFlagCommand, TextFormatting.RED, "Remove flag '" + flag + "'", ClickEvent.Action.RUN_COMMAND))
                .append(new StringTextComponent(" '" + flag + "'"));
    }

    public static IFormattableTextComponent buildHelpSuggestionLink(String translationKey, CommandConstants baseCmd, CommandConstants cmd) {
        String command = "/" + CommandConstants.BASE_CMD + " " + baseCmd + " " + cmd + " ";
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, TextFormatting.GREEN, "chat.link.hover.command.copy", ClickEvent.Action.SUGGEST_COMMAND))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static IFormattableTextComponent buildHelpLink(String translationKey, CommandConstants cmd) {
        String command =  "/" + CommandConstants.BASE_CMD + " " + cmd.toString() + " " + CommandConstants.HELP;
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent("=>", command, TextFormatting.GREEN, "Show detailed help for the " + cmd.toString() + " commands",  ClickEvent.Action.RUN_COMMAND))
                .append(new StringTextComponent(" "))
                .append(new TranslationTextComponent(translationKey));
    }

    public static IFormattableTextComponent buildHelpHeader(String translationKey){
        return new StringTextComponent(TextFormatting.BOLD + " == ")
                .append(new TranslationTextComponent(translationKey).setStyle(Style.EMPTY.withBold(true)))
                .append(new StringTextComponent(TextFormatting.BOLD + " == "));
    }

    public static IFormattableTextComponent buildHelpHeader(TranslationTextComponent translationTextComponent){
        return new StringTextComponent(TextFormatting.BOLD + " == ")
                .append(translationTextComponent)
                .append(new StringTextComponent(TextFormatting.BOLD + " == "));
    }

    // TODO: add overloading with lang-key
    public static IFormattableTextComponent buildFlagListLink(AbstractMarkableRegion region) {
        String command = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.FLAG + " " + CommandConstants.LIST + " " + region.getName();
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent(region.getFlags().size() + " flag(s)", command,
                        TextFormatting.AQUA, "List flags in region '" + region.getName() + "'", ClickEvent.Action.RUN_COMMAND));
    }

    // TODO: add overloading with lang-key
    public static IFormattableTextComponent buildAddFlagLink(String regionName) {
        String command =  "/" + CommandConstants.BASE_CMD + " " + CommandConstants.FLAG + " " + CommandConstants.ADD + " " + regionName + " ";
        return new StringTextComponent(" ").append(buildExecuteCmdComponent("+", command,
                TextFormatting.GREEN, "Add new flag to region '" + regionName + "'", ClickEvent.Action.SUGGEST_COMMAND));
    }
/*
    // TODO: add overloading with lang-key
    public static IFormattableTextComponent buildPlayerListLink(AbstractMarkableRegion region){
        String command = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.PLAYER + " " + CommandConstants.LIST + " " + region.getName();
        return new StringTextComponent(" ")
                .append(buildExecuteCmdComponent(region.getPlayers().size() + " player(s)", command,
                        TextFormatting.AQUA, "List players in region '" + region.getName() + "'"));
    }
    */

    // TODO: add overloading with lang-key
    public static IFormattableTextComponent buildAddPlayerLink(String regionName){
        String command = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.PLAYER + " " + CommandConstants.ADD + " " + regionName + " ";
        return new StringTextComponent(" ").append(buildExecuteCmdComponent("+", command,
                TextFormatting.GREEN, "Add new player to region '" + regionName + "'", ClickEvent.Action.SUGGEST_COMMAND));
    }

    // TODO: lang-keys
    public static IFormattableTextComponent buildRegionPriorityInfoLink(String regionName, int regionPriority) {
        String baseCommand = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.REGION + " " + CommandConstants.PRIORITY + " " + regionName + " ";
        String setCommand = baseCommand + regionPriority;
        String incrementCommand = baseCommand + (regionPriority + REGION_DEFAULT_PRIORITY_INC.get());
        String decrementCommand = baseCommand + (regionPriority - REGION_DEFAULT_PRIORITY_INC.get());
        return new TranslationTextComponent("message.region.info.priority", regionPriority)
                .append(new StringTextComponent(" "))
                .append(buildExecuteCmdComponent("#", setCommand,
                        TextFormatting.GREEN, "Set new priority for region '" + regionName + "'", ClickEvent.Action.SUGGEST_COMMAND))
                .append(new StringTextComponent(" "))
                .append(buildExecuteCmdComponent("+", incrementCommand,
                        TextFormatting.GREEN, "Increment region priority by " + REGION_DEFAULT_PRIORITY_INC.get(), ClickEvent.Action.RUN_COMMAND))
                .append(new StringTextComponent(" "))
                .append(buildExecuteCmdComponent("-", decrementCommand,
                        TextFormatting.RED, "Decrement region priority by " + REGION_DEFAULT_PRIORITY_INC.get(), ClickEvent.Action.RUN_COMMAND));
    }

    // TODO: lang key
    public static IFormattableTextComponent buildRegionInfoLink(String regionName){
        String command =  "/" + CommandConstants.BASE_CMD + " " + CommandConstants.REGION + " " + CommandConstants.INFO + " " + regionName;
        return buildExecuteCmdComponent(regionName, command, TextFormatting.GREEN,
                "Show region info for region '" + regionName + "'", ClickEvent.Action.RUN_COMMAND);
    }

    public static IFormattableTextComponent buildRegionMuteLink(AbstractMarkableRegion region){
        boolean isMuted = region.isMuted();
        IFormattableTextComponent linkText = isMuted
                ? new TranslationTextComponent("message.region.info.muted.true")
                : new TranslationTextComponent("message.region.info.muted.false");
        TextFormatting color = isMuted
                ? TextFormatting.RED
                : TextFormatting.GREEN;
        String onClickAction = isMuted ? "unmute" : "mute";
        String command = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.REGION + " " + onClickAction + " " + region.getName();
        // TODO: translatable overload (linkText) ?
        return buildExecuteCmdComponent(linkText.getString(), command, color, onClickAction + " " + CommandConstants.REGION + " '" + region.getName() + "'", ClickEvent.Action.RUN_COMMAND);
    }

    public static IFormattableTextComponent buildRegionActiveLink(AbstractMarkableRegion region){
        boolean isActive = region.isActive();
        IFormattableTextComponent activeText = isActive
                ? new TranslationTextComponent("message.region.info.active.true")
                : new TranslationTextComponent("message.region.info.active.false");
        TextFormatting color = isActive
                ? TextFormatting.GREEN
                : TextFormatting.RED;
        String onClickAction = isActive ? "deactivate" : "activate";
        String command = "/" + CommandConstants.BASE_CMD + " " + CommandConstants.REGION + " " + onClickAction + " " + region.getName();
        // TODO: translatable overload (linkText) ?
        return buildExecuteCmdComponent(activeText.getString(), command, color, onClickAction + " " + CommandConstants.REGION + " '" + region.getName() + "'", ClickEvent.Action.RUN_COMMAND);
    }
}
