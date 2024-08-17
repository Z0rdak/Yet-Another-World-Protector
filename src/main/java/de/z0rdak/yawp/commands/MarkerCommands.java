package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.Text;

import java.util.Collections;
import java.util.Objects;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.getRandomExample;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.Formatting.RED;

public final class MarkerCommands {

    private MarkerCommands() {
    }

    public static LiteralArgumentBuilder<ServerCommandSource> build() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(MarkerCommands::giveMarkerStick))
                .then(literal(RESET)
                        .executes(MarkerCommands::resetStick))
                .then(literal(CREATE)
                        .then(CommandManager.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList(getRandomExample()), builder))
                                .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx), null))
                                .then(CommandManager.argument(PARENT.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestionsWithMarker(ctx, builder))
                                        .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx), getContainingOwnedRegionArgumentWithMarker(ctx))))));
    }

    public static IMarkableRegion fromMarkedBlocks(CommandContext<ServerCommandSource> ctx, PlayerEntity player, String regionName) throws CommandSyntaxException {
        ItemStack maybeStick = player.getMainHandStack();
        if (StickUtil.isVanillaStick(maybeStick)) {
            StickType stickType = StickUtil.getStickType(maybeStick);
            if (stickType == StickType.MARKER) {
                NbtCompound stickNBT = StickUtil.getStickNBT(maybeStick);
                if (stickNBT != null) {
                    MarkerStick marker = new MarkerStick(stickNBT);
                    if (!marker.isValidArea()) {
                        sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.area.invalid", "Marked area is not valid").formatted(RED));
                        return null;
                    }
                    return LocalRegions.regionFrom(player, marker, regionName);
                } else {
                    sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.invalid", "Invalid RegionMarker data, sorry. Get a new one and try again."));
                    return null;
                }
            } else {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing", "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
                return null;
            }
        } else {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing", "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
            return null;
        }
    }

    private static int createMarkedRegion(CommandContext<ServerCommandSource> ctx, String regionName, IProtectedRegion parentRegion) {
        try {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(ctx.getSource().getWorld().getRegistryKey());
            int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
            if (res == -1) {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
                return res;
            }
            if (res == 1) {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", buildRegionInfoLink(dimCache.getDimensionalRegion()), buildRegionInfoLink(dimCache.getRegion(regionName))));
                return res;
            }
            PlayerEntity player = ctx.getSource().getPlayerOrThrow();
            IMarkableRegion newRegion = fromMarkedBlocks(ctx, player, regionName);
            if (newRegion == null) {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.area.invalid", "Marked area is not valid").formatted(RED));
                return -1;
            }
            IProtectedRegion parent = parentRegion == null ? dimCache.getDimensionalRegion() : parentRegion;
            return createRegion(ctx, player, dimCache, newRegion, parent);            
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player", "This command can only be executed as a player!").withStyle(RED));
            return -1;
        }
    }

    private static int createRegion(CommandContext<ServerCommandSource> ctx, PlayerEntity player, DimensionRegionCache dimCache, IMarkableRegion region, IProtectedRegion parentRegion) {
        if (RegionEvents.CREATE_REGION.invoker().createRegion(new RegionEvent.CreateRegionEvent(region, player))) {
            return 1;
        }
        boolean hasConfigPermission = CommandPermissionConfig.hasConfigPermission(player);
        boolean hasRegionPermission = CommandPermissionConfig.hasRegionPermission(parentRegion, player, CommandUtil.OWNER);
        if (hasConfigPermission || hasRegionPermission) {
            RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
            dimCache.addRegion(parentRegion, region);
            LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.getDefaultPriority());
            RegionDataManager.save();
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.success", "Successfully created region %s (with parent %s)", buildRegionInfoLink(region), buildRegionInfoLink(parentRegion)));
            return 0;
        } else {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.local.deny", "You don't have the permission to create a region in the region %s!", buildRegionInfoLink(parentRegion)));
            return 1;
        }
    }

    private static int resetStick(CommandContext<ServerCommandSource> ctx) {
        try {
            PlayerEntity player = ctx.getSource().getPlayerOrThrow();
            ItemStack mainHandItem = player.getMainHandStack();
            // is valid stick
            if (!mainHandItem.equals(ItemStack.EMPTY)
                    && StickUtil.hasNonNullTag(mainHandItem)
                    && mainHandItem.getNbt().contains(STICK)) {
                StickType stickType = getStickType(mainHandItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    mainHandItem = StickUtil.initMarkerNbt(mainHandItem, StickType.MARKER, player.getWorld().getRegistryKey());
                    // Note: When different area types are available: Get stick, reset it, and save it back.
                    sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.reset", "RegionMarker successfully reset!"));
                    return 0;
                } else {
                    sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing", "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
                    return 1;
                }
            } else {
                sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing", "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player", "This command can only be executed as a player!").formatted(RED));
            return 1;
        }
    }

    public static int giveMarkerStick(CommandContext<ServerCommandSource> ctx) {
        try {
            PlayerEntity targetPlayer = ctx.getSource().getPlayerOrThrow();
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultStack(), StickType.MARKER, targetPlayer.getWorld().getRegistryKey());
            targetPlayer.giveItemStack(markerStick);
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.success", "RegionMarker added to your inventory!"));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(ctx.getSource(), Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player", "This command can only be executed as a player!").formatted(RED));
            return 1;
        }
        return 0;
    }
}
