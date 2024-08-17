package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.common.MinecraftForge;

import java.util.Collections;
import java.util.Objects;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.getRandomExample;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatComponentBuilder.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.text.TextFormatting.RED;

public final class MarkerCommands {

    private MarkerCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> build() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(MarkerCommands::giveMarkerStick))
                .then(literal(RESET)
                        .executes(MarkerCommands::resetStick))
                .then(literal(CREATE)
                        .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(Collections.singletonList(getRandomExample()), builder))
                                .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx), null))
                                .then(Commands.argument(PARENT.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestionsWithMarker(ctx, builder))
                                        .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx), getContainingOwnedRegionArgumentWithMarker(ctx))))));
    }

    public static IMarkableRegion fromMarkedBlocks(CommandContext<CommandSource> ctx, PlayerEntity player, String regionName) throws CommandSyntaxException {
        ItemStack maybeStick = player.getMainHandItem();
        if (StickUtil.isVanillaStick(maybeStick)) {
            StickType stickType = StickUtil.getStickType(maybeStick);
            if (stickType == StickType.MARKER) {
                CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                if (stickNBT != null) {
                    MarkerStick marker = new MarkerStick(stickNBT);
                    if (!marker.isValidArea()) {
                        sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.area.invalid").withStyle(RED));
                        return null;
                    }
                    return LocalRegions.regionFrom(player, marker, regionName);
                } else {
                    sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.invalid"));
                    return null;
                }
            } else {
                sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                return null;
            }
        } else {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
            return null;
        }
    }

    private static int createMarkedRegion(CommandContext<CommandSource> ctx, String regionName, IProtectedRegion parentRegion) {
        try {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(ctx.getSource().getLevel().dimension());
            int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
            if (res == -1) {
                sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
                return res;
            }
            if (res == 1) {
                sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", buildRegionInfoLink(dimCache.getDimensionalRegion()), buildRegionInfoLink(dimCache.getRegion(regionName))));
                return res;
            }
            PlayerEntity player = ctx.getSource().getPlayerOrException();
            IMarkableRegion newRegion = fromMarkedBlocks(ctx, player, regionName);
            if (newRegion == null) {
                sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.area.invalid").withStyle(RED));
                return -1;
            }
            IProtectedRegion parent = parentRegion == null ? dimCache.getDimensionalRegion() : parentRegion;
            return createRegion(ctx, player, dimCache, newRegion, parent);
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.no-player").withStyle(RED));
            return -1;
        }
    }

    private static int createRegion(CommandContext<CommandSource> ctx, PlayerEntity player, DimensionRegionCache dimCache, IMarkableRegion region, IProtectedRegion parentRegion) {
        if (MinecraftForge.EVENT_BUS.post(new RegionEvent.CreateRegionEvent(region, player))) {
            return 1;
        }
        boolean hasConfigPermission = CommandPermissionConfig.hasConfigPermission(player);
        boolean hasRegionPermission = CommandPermissionConfig.hasRegionPermission(parentRegion, player, CommandUtil.OWNER);
        if (hasConfigPermission || hasRegionPermission) {
            RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
            dimCache.addRegion(parentRegion, region);
            LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.getDefaultPriority());
            RegionDataManager.save();
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region), buildRegionInfoLink(parentRegion)));
            return 0;
        } else {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.local.deny", buildRegionInfoLink(parentRegion)));
            return 1;
        }
    }

    private static int resetStick(CommandContext<CommandSource> ctx) {
        try {
            PlayerEntity player = ctx.getSource().getPlayerOrException();
            ItemStack mainHandItem = player.getMainHandItem();
            // is valid stick
            if (!mainHandItem.equals(ItemStack.EMPTY)
                    && StickUtil.hasNonNullTag(mainHandItem)
                    && mainHandItem.getTag().contains(STICK)) {
                StickType stickType = getStickType(mainHandItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    mainHandItem = StickUtil.initMarkerNbt(mainHandItem, StickType.MARKER, player.getCommandSenderWorld().dimension());
                    // Note: When different area types are available: Get stick, reset it, and save it back.
                    sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.reset"));
                    return 0;
                } else {
                    sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                    return 1;
                }
            } else {
                sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.no-player").withStyle(RED));
            return 1;
        }
    }

    public static int giveMarkerStick(CommandContext<CommandSource> ctx) {
        try {
            PlayerEntity targetPlayer = ctx.getSource().getPlayerOrException();
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultInstance(), StickType.MARKER, targetPlayer.level.dimension());
            targetPlayer.addItem(markerStick);
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.success"));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(ctx.getSource(), new TranslationTextComponent("cli.msg.dim.info.region.create.stick.no-player").withStyle(RED));
            return 1;
        }
        return 0;
    }
}
