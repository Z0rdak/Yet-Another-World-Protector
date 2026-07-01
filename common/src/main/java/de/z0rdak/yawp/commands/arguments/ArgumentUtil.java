package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import de.z0rdak.yawp.api.Flag;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.visuals.DisplayType;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;

import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.data.region.LevelData;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.IdentifierArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

public class ArgumentUtil {

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> Component.translatableWithFallback(
                    "cli.arg.region.invalid",
                    "Region '%s' does not exist", flag));

    public static final DynamicCommandExceptionType ERROR_LOCAL_REGION_EXPECTED = new DynamicCommandExceptionType(
            region -> Component.translatableWithFallback(
                    "cli.arg.region.local.expected",
                    "Invalid region supplied (%s). Expected Local Region.", region));

    public static final DynamicCommandExceptionType ERROR_GLOBAL_NOT_ALLOWED = new DynamicCommandExceptionType(
            region -> Component.translatableWithFallback("cli.arg.region.global.not-allowed", "Invalid region supplied. Only Dimensional and Local Region allowed here.", region));

    public static final DynamicCommandExceptionType ERROR_LEVEL_REGION_EXPECTED = new DynamicCommandExceptionType(
            region -> Component.translatableWithFallback(
                    "cli.arg.region.dim.expected",
                    "Invalid region supplied (%s). Expected Dimensional Region.", region));

    public static final DynamicCommandExceptionType ERROR_INVALID_LEVEL = new DynamicCommandExceptionType(
            flag -> Component.translatableWithFallback(
                    "cli.arg.region.invalid",
                    "Unable to find dimension data", flag));

    public static final DynamicCommandExceptionType ERROR_REGION_ID_INVALID = new DynamicCommandExceptionType(
            regionIdentifier -> Component.translatableWithFallback(
                    "cli.arg.region.identifier.invalid",
                    "Supplied region identifier '%s' is invalid.", regionIdentifier));

    public static final DynamicCommandExceptionType ERROR_LEVEL_INVALID_VALUE =
            new DynamicCommandExceptionType(regionId -> Component.translatableWithFallback(
                    "cli.arg.region.level.invalid",
                    "Region for level '%s' does not exist or is not tracked.", regionId));

    public static final DynamicCommandExceptionType ERROR_REGION_NOT_UNIQUE = new DynamicCommandExceptionType(
            regionIdentifier -> Component.translatableWithFallback(
                    "cli.arg.region.local.name.ambiguous",
                    "More regions named '%s' across multiple levels found. Please specify level.", regionIdentifier));

    public static LiteralArgumentBuilder<CommandSourceStack> literal(CommandConstants constant) {
        return Commands.literal(constant.toString());
    }

    public static GlobalRegion getGlobalRegion() {
        return RegionManager.get().getGlobalRegion();
    }

    public static String getRegionNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.NAME.toString());
    }

    public static Identifier getRegionIdArgument(CommandContext<CommandSourceStack> ctx) {
        return IdentifierArgument.getId(ctx, CommandConstants.NAME.toString());
    }

    public static DisplayType getDisplayTypeArgument(CommandContext<CommandSourceStack> ctx) {
        String displayType = StringArgumentType.getString(ctx, STYLE.toString());
        return DisplayType.of(displayType);
    }

    public static Identifier getDisplayBlockArgument(CommandContext<CommandSourceStack> ctx) {
        return IdentifierArgument.getId(ctx, CommandConstants.BLOCK.toString());
    }

    public static BlockPos getTeleportAnchorPosArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return BlockPosArgument.getSpawnablePos(ctx, TP_ANCHOR.toString());
    }

    public static String getTeleportAnchorNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.NAME.toString());
    }

    public static String getNewTeleportAnchorNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.RENAME.toString());
    }

    public static boolean getDisplayGlowArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.GLOW.toString());
    }

    public static UUID getRequestId(CommandContext<CommandSourceStack> ctx) {
        return UuidArgument.getUuid(ctx, REQUEST_ID.toString());
    }


    private static IMarkableRegion resolveLocalRegion(String identifier) throws CommandSyntaxException {
        var parts = identifier.split("/");
        return switch (parts.length) {
            case 1 -> resolveUniqueLocalRegion(identifier);
            case 2 -> resolveLocalRegion(parts[0], parts[1]);
            default -> throw ERROR_REGION_ID_INVALID.create(identifier);
        };
    }

    private static IMarkableRegion resolveUniqueLocalRegion(String regionName) throws CommandSyntaxException {
        IMarkableRegion found = null;
        int occurrences = 0;
        for (var level : RegionManager.get().getLevels()) {
            var lrd = RegionManager.get().getLevelRegionData(level).orElseThrow();
            if (lrd.hasLocal(regionName)) {
                occurrences++;
                if (occurrences > 1) {
                    throw ERROR_REGION_NOT_UNIQUE.create(regionName);
                }
                found = lrd.getLocal(regionName);
            }
        }
        if (found == null) {
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        return found;
    }

    private static IMarkableRegion resolveLocalRegion(String levelId, String regionName) throws CommandSyntaxException {
        var levelRegion = resolveRootRegion(Identifier.parse(levelId));
        if (levelRegion == null) {
            throw ERROR_LEVEL_INVALID_VALUE.create(levelId);
        }
        var lrd = RegionManager.get()
                .getLevelRegionData(levelRegion.getDim())
                .orElseThrow();
        if (!lrd.hasLocal(regionName)) {
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        return lrd.getLocal(regionName);
    }


    public static IMarkableRegion getLocalChildRegion(CommandContext<CommandSourceStack> context, String argName, IProtectedRegion parent) throws CommandSyntaxException {
        var childName = StringArgumentType.getString(context, argName);
        var parentLevel = parent.getDim().identifier();
        var childId = Identifier.parse(parentLevel + "/" + childName);
        if (resolveRootRegion(childId) instanceof GlobalRegion) {
            throw ERROR_GLOBAL_NOT_ALLOWED.create(parent.getId().toString());
        }
        return resolveLocalRegion(childId.toString());
    }

    public static IMarkableRegion getLocalRegion(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        var id = IdentifierArgument.getId(context, argName);
        if (resolveRootRegion(id) != null) {
            throw ERROR_LOCAL_REGION_EXPECTED.create(id.toString());
        }
        return resolveLocalRegion(id.toString());
    }

    @Nullable
    private static IProtectedRegion resolveRootRegion(Identifier id) {
        if (GlobalRegion.GLOBAL.equals(id)) {
            return RegionManager.get().getGlobalRegion();
        }
        return RegionManager.get().getLevelRegionData(id)
                .map(LevelData::getDim)
                .orElse(null);
    }
    public static LevelData getLevelDataFor(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        var regionIdRl = IdentifierArgument.getId(context, argName);
        var regionIdentifier = regionIdRl.toString();
        var region = resolveRootRegion(regionIdRl);
        if (region == null){
            throw ERROR_LEVEL_INVALID_VALUE.create(regionIdentifier);
        }
        if (region.getRegionType() != RegionType.DIMENSION)
            throw ERROR_LEVEL_REGION_EXPECTED.create(regionIdentifier);
        var maybeLevelData = RegionDataManager.getLevelRegionData(regionIdRl);
        if (maybeLevelData.isEmpty()) {
            // TODO: CommandLink to track
            sendCmdFeedback(context.getSource(), Component.translatableWithFallback("cli.msg.global.level-not-tracked", "The level '%s' is currently not tracked by YAWP. Track it by using %s", regionIdRl, "cmd"));
            throw ERROR_LEVEL_INVALID_VALUE.create(regionIdentifier);
        }
        return maybeLevelData.get();
    }

    public static IProtectedRegion getRegion(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        var id = IdentifierArgument.getId(context, argName);
        var region = resolveRootRegion(id);
        if (region != null) {
            return region;
        }
        return resolveLocalRegion(id.toString());
    }


    public static IMarkableRegion getLocalRegionArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return getLocalRegion(ctx, REGION.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getRegionArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return getRegion(ctx, REGION.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getParentRegionArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return getRegion(ctx, PARENT.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getTargetRegionArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return getRegion(ctx, TARGET.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }


    public static IProtectedRegion getSubRegionArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return getRegion(ctx, SUBREGION.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static LevelData getLevelDataArgument(CommandContext<CommandSourceStack> ctx) {
        try {
            return getLevelDataFor(ctx, REGION.toString());
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IMarkableRegion getRegionIn(CommandContext<CommandSourceStack> ctx, Level level) {
        try {
            String regionName = ctx.getArgument(LOCAL.toString(), String.class);
            var dimensionCache = RegionManager.get().getLevelRegionData(level.dimension());
            if (dimensionCache.isPresent()) {
                var dimCache = dimensionCache.get();
                if (!dimCache.hasLocal(regionName)) {
                    sendCmdFeedback(ctx.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.getDim().getName() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
                IMarkableRegion region = dimCache.getLocal(regionName);
                if (region != null) {
                    return region;
                } else {
                    sendCmdFeedback(ctx.getSource(), Component.literal("No regions defined in dim '" + dimCache.getDim().getName() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
            } else {
                throw ERROR_INVALID_LEVEL.create(level.dimension().identifier().toString());
            }
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public static IProtectedRegion getRegion(CommandContext<CommandSourceStack> ctx, RegionType regionType) throws CommandSyntaxException {
        switch (regionType) {
            case GLOBAL:
                return RegionManager.get().getGlobalRegion();
            case DIMENSION: {
                LevelData dimCache = ArgumentUtil.getLevelDataArgument(ctx);
                return dimCache.getDim();
            }
            case LOCAL: {
                LevelData dimCache = ArgumentUtil.getLevelDataArgument(ctx);
                String regionName = ctx.getArgument(CommandConstants.LOCAL.toString(), String.class);
                if (!dimCache.hasLocal(regionName)) {
                    sendCmdFeedback(ctx.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.getDim().getName() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
                IMarkableRegion region = dimCache.getLocal(regionName);
                if (region != null) {
                    return region;
                } else {
                    sendCmdFeedback(ctx.getSource(), Component.literal("No regions defined in dim '" + dimCache.getDim().getName() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
            }
            default:
                throw ERROR_INVALID_VALUE.create("");
        }
    }

    public static IProtectedRegion getTargetLocalRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        String regionName = ctx.getArgument(TARGET_REGION.toString(), String.class);
        LevelData dimCache = ArgumentUtil.getTargetDimRegionArgument(ctx);
        if (!dimCache.hasLocal(regionName)) {
            sendCmdFeedback(ctx.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.getDim().getName() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getLocal(regionName);
        if (region != null) {
            return region;
        } else {
            sendCmdFeedback(ctx.getSource(), Component.literal("No regions defined in dim '" + dimCache.getDim().getName() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    public static LevelData getTargetDimRegionArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return getDimRegion(ctx, TARGET_DIM.toString());
    }
    public static LevelData getDimRegion(CommandContext<CommandSourceStack> context, String dim) throws CommandSyntaxException {
        Identifier levelRl = context.getArgument(dim, Identifier.class);
        boolean isValidDimIdentifier = context.getSource().levels().stream()
                .map(ResourceKey::identifier)
                .anyMatch(loc -> loc.equals(levelRl));
        if (isValidDimIdentifier) {
            var maybeLevelData = RegionDataManager.getLevelRegionData(levelRl);
            if (maybeLevelData.isEmpty()) {
                // TODO: CommandLink
                sendCmdFeedback(context.getSource(), Component.translatableWithFallback("cli.msg.global.level-not-tracked", "The level '%s' is currently not tracked by YAWP. Track it by using %s", levelRl, "cmd"));
                return null;
            }
            return maybeLevelData.get();
        } else {
            //throw ERROR_INVALID_VALUE.create(levelRl.toString());
            return null;
        }
    }

    public static IMarkableRegion getChildRegionArgument(CommandContext<CommandSourceStack> ctx, IProtectedRegion parent) throws CommandSyntaxException {
        return getLocalChildRegion(ctx, CHILD.toString(), parent);
    }

    public static ServerPlayer getPlayerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.PLAYER.toString());
    }

    public static Collection<ServerPlayer> getPlayersArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayers(ctx, CommandConstants.PLAYER.toString());
    }

    public static java.util.UUID getPlayerUUIDArgument(CommandContext<CommandSourceStack> ctx) {
        return UuidArgument.getUuid(ctx, CommandConstants.PLAYER_UUID.toString());
    }

    public static List<String> getPlayerNamesArgument(CommandContext<CommandSourceStack> ctx) {
        String[] names = StringArgumentType.getString(ctx, PLAYER_NAMES.toString()).split(" ");
        return Arrays.asList(names);
    }

    public static String getFlagNameArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.FLAG.toString());
    }

    public static Flag getFlagArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var flagIdentifier = IdentifierArgument.getId(ctx, CommandConstants.FLAG.toString());
        try {
            return FlagRegister.byId(flagIdentifier);
        } catch (IllegalArgumentException e) {
            sendCmdFeedback(ctx.getSource(), Component.literal("Invalid flag identifier: '" + flagIdentifier + "'!"));
            // throw IFlagArgumentType.ERROR_INVALID_VALUE.create(flagIdentifier);
            // TODO send error
            return null;
        }
    }

    public static Set<Flag> getFlagArguments(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        String flagIdentifiers = ctx.getArgument(CommandConstants.FLAGS.toString(), String.class);
        Set<String> flagsList = new HashSet<>(Arrays.asList(flagIdentifiers.split(" ")));
        Set<Flag> regionFlags = flagsList.stream()
                .filter(flag -> {
                    if (FlagRegister.isRegistered(flag))
                        return true;
                    else {
                        sendCmdFeedback(ctx.getSource(), Component.literal("Invalid flag identifier: '" + flag + "'!"));
                        return false;
                    }
                })
                .map(FlagRegister::byId)
                .collect(Collectors.toSet());
        if (regionFlags.isEmpty()) {
            // throw IFlagArgumentType.ERROR_INVALID_VALUE.create(flagIdentifiers);
            // TODO send error
            return null;
        }
        return regionFlags;
    }

    @Nullable
    public static IFlag getIFlagArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var flagIdentifier = ctx.getArgument(CommandConstants.FLAG.toString(), Identifier.class);
        try {
            var flag = FlagRegister.byId(flagIdentifier);
            var region = getLocalRegion(ctx, CommandConstants.REGION.toString());
            if (region.containsFlag(flag)) {
                return region.getFlag(flag.name());
            } else {
                MutableComponent flagAddHint = Component.translatableWithFallback("cli.msg.info.region.flag.add-hint", "Add flag by clicking: %s", ChatLinkBuilder.buildAddFlagLink(region, flag.name()));
                MutableComponent flagNotPresentInfo = Component.translatableWithFallback("cli.msg.info.region.flag.not-present", "Region %s does not contain flag '%s'. %s", buildRegionInfoLink(region), flagIdentifier, flagAddHint);
                sendCmdFeedback(ctx.getSource(), flagNotPresentInfo);
                return null;
            }
        }catch (IllegalArgumentException e) {
            sendCmdFeedback(ctx.getSource(), Component.literal("Invalid flag identifier: '" + flagIdentifier + "'!"));
            //  TODO just send error?
            // throw IFlagArgumentType.ERROR_INVALID_VALUE.create(flagIdentifier);
            return null;
        }
    }

    public static String getFlagMsgArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.MSG.toString());
    }

    public static String getGroupArgument(CommandContext<CommandSourceStack> ctx) {
        return StringArgumentType.getString(ctx, CommandConstants.GROUP.toString());
    }

    public static ServerPlayer getOwnerArgument(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return EntityArgument.getPlayer(ctx, CommandConstants.OWNER.toString());
    }

    public static boolean getAlertArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ALERT.toString());
    }

    public static boolean muteArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.MUTE.toString());
    }


    public static boolean getEnableArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.ENABLE.toString());
    }

    public static FlagState getFlagStateArgument(CommandContext<CommandSourceStack> ctx) {
        String state = StringArgumentType.getString(ctx, STATE.toString());
        return FlagState.from(state);
    }

    public static boolean getOverrideArgument(CommandContext<CommandSourceStack> ctx) {
        return BoolArgumentType.getBool(ctx, CommandConstants.OVERRIDE.toString());
    }

    public static int getPriorityArgument(CommandContext<CommandSourceStack> ctx) {
        return IntegerArgumentType.getInteger(ctx, CommandConstants.PRIORITY.toString());
    }

    public static int getPageNoArgument(CommandContext<CommandSourceStack> ctx) {
        return IntegerArgumentType.getInteger(ctx, PAGE.toString());
    }

    public static String revertCommand(String cmd, CommandConstants toReplace, CommandConstants reverted) {
        String revertedCmd = cmd.replace(toReplace.toString(), reverted.toString());
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd;
    }

    public static String revertCommand(String cmd, String toReplace, String reverted) {
        String revertedCmd = cmd.replace(toReplace, reverted);
        return cmd.startsWith("/") ? revertedCmd : "/" + revertedCmd;
    }
}
