package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.commands.arguments.region.ContainingOwnedRegionArgumentType;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

import java.util.Collections;

import static de.z0rdak.yawp.api.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.getRandomExample;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static net.minecraft.ChatFormatting.RED;

public final class MarkerCommands {

    private MarkerCommands() {
    }

    static LiteralArgumentBuilder<CommandSourceStack> build() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(MarkerCommands::giveMarkerStick))
                .then(literal(RESET)
                        .executes(MarkerCommands::resetStick))
                .then(literal(SELECT)
                        .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                                .then(Commands.argument(LOCAL.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> selectRegion(ctx, getRegionArgument(ctx), true)))))
                .then(literal(DESELECT)
                        .executes(ctx -> selectRegion(ctx, getRegionArgument(ctx), false)))
                .then(literal(CREATE)
                        .then(Commands.argument(CommandConstants.NAME.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList(getRandomExample()), builder))
                                .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx), null))
                                .then(Commands.argument(PARENT.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ContainingOwnedRegionArgumentType.owningRegions().listSuggestionsWithMarker(ctx, builder))
                                        .executes(ctx -> createMarkedRegion(ctx, getRegionNameArgument(ctx), getContainingOwnedRegionArgumentWithMarker(ctx))))));
    }

    private static int selectRegion(CommandContext<CommandSourceStack> ctx, IMarkableRegion region, boolean select) {
      
        return 0;
    }

    // TODO: Move into Util
    public static IMarkableRegion fromMarkedBlocks(CommandContext<CommandSourceStack> ctx, Player player, String regionName) throws CommandSyntaxException {
        ItemStack maybeStick = player.getMainHandItem();
        if (StickUtil.isMarker(maybeStick)) {
            CompoundTag stickNBT = StickUtil.getStickNBT(maybeStick);
            if (stickNBT != null) {
                MarkerStick marker = new MarkerStick(stickNBT);
                if (!marker.isValidArea()) {
                    sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.area.invalid", "Marked area is not valid").withStyle(RED));
                    return null;
                }
                return LocalRegions.regionFrom(player, marker, regionName);
            } else {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.invalid", "Invalid RegionMarker data, sorry. Get a new one and try again."));
                return null;
            }
            
        } else {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing", "Put a valid(*) RegionMarker in your main hand to create a region!").withStyle(RED));
            return null;
        }
    }

    private static int createMarkedRegion(CommandContext<CommandSourceStack> ctx, String regionName, IProtectedRegion parentRegion) {
        try {
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(ctx.getSource().getLevel().dimension());
            int res = RegionDataManager.get().isValidRegionName(dimCache.getDimensionalRegion().getDim(), regionName);
            if (res == -1) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid", "Invalid region name supplied: '%s'", regionName));
                return res;
            }
            if (res == 1) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.name.exists", "Dimension %s already contains region with name %s", buildRegionInfoLink(dimCache.getDimensionalRegion()), buildRegionInfoLink(dimCache.getRegion(regionName))));
                return res;
            }
            Player player = ctx.getSource().getPlayerOrException();
            IMarkableRegion newRegion = fromMarkedBlocks(ctx, player, regionName);
            if (newRegion == null) {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.area.invalid", "Marked area is not valid").withStyle(RED));
                return -1;
            }
            IProtectedRegion parent = parentRegion == null ? dimCache.getDimensionalRegion() : parentRegion;
            return createRegion(ctx, player, dimCache, newRegion, parent);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error(e);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player", "This command can only be executed as a player!").withStyle(RED));
            return -1;
        }
    }

    private static int createRegion(CommandContext<CommandSourceStack> ctx, Player player, DimensionRegionCache dimCache, IMarkableRegion region, IProtectedRegion parentRegion) {
        if (Services.EVENT.post(new RegionEvent.Create(region, player)) ) {
            return 1;
        }
        //if (RegionEvents.CREATE_REGION.invoker().createRegion(new RegionEvent.CreateRegionEvent(region, player))) {
        //    return 1;
        //}
        boolean hasConfigPermission = Permissions.get().hasConfigPermission(player);
        boolean hasRegionPermission = Permissions.get().hasGroupPermission(parentRegion, player, Permissions.OWNER);
        if (hasConfigPermission || hasRegionPermission) {
            RegionDataManager.addFlags(Services.REGION_CONFIG.getDefaultFlags(), region);
            dimCache.addRegion(parentRegion, region);
            LocalRegions.ensureHigherRegionPriorityFor(region, Services.REGION_CONFIG.getDefaultPriority());
            RegionDataManager.save();
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.success", "Successfully created region %s (with parent %s)", buildRegionInfoLink(region), buildRegionInfoLink(parentRegion)));
            return 0;
        } else {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.local.deny", "You don't have the permission to create a region in the region %s!", buildRegionInfoLink(parentRegion)));
            return 1;
        }
    }

    private static int resetStick(CommandContext<CommandSourceStack> ctx) {
        try {
            Player player = ctx.getSource().getPlayerOrException();
            ItemStack mainHandItem = player.getMainHandItem();
            if (!mainHandItem.equals(ItemStack.EMPTY) && StickUtil.isMarker(mainHandItem)) {
                StickUtil.resetMarkerNbt(mainHandItem, player.level().dimension());
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.reset", "RegionMarker successfully reset!"));
                return 0;
            } else {
                sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing", "Put a valid(*) RegionMarker in your main hand to create a region!").withStyle(RED));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player", "This command can only be executed as a player!").withStyle(RED));
            return 1;
        }
    }

    private static int giveMarkerStick(CommandContext<CommandSourceStack> ctx) {
        try {
            Player targetPlayer = ctx.getSource().getPlayerOrException();
            ItemStack marker = Items.STICK.getDefaultInstance();
            StickUtil.initMarkerNbt(marker, targetPlayer.level().dimension());
            targetPlayer.addItem(marker);
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.success", "RegionMarker added to your inventory!"));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player", "This command can only be executed as a player!").withStyle(RED));
            return 1;
        }
        return 0;
    }
}
