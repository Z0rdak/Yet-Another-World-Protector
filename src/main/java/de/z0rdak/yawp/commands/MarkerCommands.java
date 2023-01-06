package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.CuboidRegion;
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
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Objects;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.getPlayerArgument;
import static de.z0rdak.yawp.util.CommandUtil.literal;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.text.TextFormatting.RED;

public final class MarkerCommands {

    public static final LiteralArgumentBuilder<CommandSource> MARKER_COMMAND = register();

    private MarkerCommands() {
    }

    private static LiteralArgumentBuilder<CommandSource> register() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(ctx -> giveMarkerStick(ctx.getSource(), null))
                        .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                .executes(ctx -> giveMarkerStick(ctx.getSource(), getPlayerArgument(ctx)))))
                /*
                .then(literal(SET)
                        .then(Commands.argument(PARENT_REGION.toString(), StringArgumentType.word()))
                        .executes(ctx -> setParent(ctx.getSource(), getParentRegionArgument(ctx))))
                 */
                .then(literal(RESET)
                        .executes(ctx -> resetStick(ctx.getSource())))
                /*
                .then(literal(CREATE)
                        .then(Commands.argument(NAME.toString(), StringArgumentType.word())
                                .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), null)))
                        .then(Commands.argument(PARENT.toString(), StringArgumentType.word())
                                .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getParentRegionArgument(ctx)))))
                 */
                ;
    }


    private static int createRegion(CommandSource src, String regionName, DimensionRegionCache dimCache, IProtectedRegion parentRegion) {
        int res = DimensionCommands.checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), regionName));
            return res;
        }
        // TODO: Check if player is allowed to create region - here or in command interceptor?

        try {
            PlayerEntity player = src.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            // TODO: create a method which throws exception on trying to get stick
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        AbstractMarkableRegion region = LocalRegions.regionFrom(src.getPlayerOrException(), new MarkerStick(stickNBT), regionName);
                        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
                        dimCache.addRegion(region);
                        LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                        RegionDataManager.save();
                        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));


                    } else {
                        // stick data corrupt
                    }
                } else {
                    // Player not holding correct stick in hand
                }
            } else {
                // Player not holding stick in hand
                sendCmdFeedback(src, new StringTextComponent(RED + "").append(new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing")));
                return -2;
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
        return 0;
    }


    private static int resetStick(CommandSource src) {
        try {
            PlayerEntity player = src.getPlayerOrException();
            ItemStack mainHandItem = player.getMainHandItem();
            // is valid stick
            if (!mainHandItem.equals(ItemStack.EMPTY)
                    && StickUtil.hasNonNullTag(mainHandItem)
                    && mainHandItem.getTag().contains(STICK)) {
                StickType stickType = getStickType(mainHandItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    mainHandItem = StickUtil.initMarkerNbt(mainHandItem, StickType.MARKER, player.getCommandSenderWorld().dimension());
                    // FIXME: When different area types are available: Get stick, reset it, and save it back.
                    sendCmdFeedback(src, new TranslationTextComponent("RegionMarker reset!"));
                    return 0;
                } else {
                    sendCmdFeedback(src, new TranslationTextComponent(RED + "The item in the main hand is not a RegionMarker!"));
                    return 1;
                }
            } else {
                sendCmdFeedback(src, new TranslationTextComponent(RED + "The item in the main hand is not a RegionMarker!"));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, new TranslationTextComponent(RED + "Command needs a player as command source" + RESET));
            return 1;
        }
    }

    private static int setParent(CommandSource src, IMarkableRegion parent) {
        return -1;
    }

    public static int giveMarkerStick(CommandSource src, ServerPlayerEntity player) {
        try {
            PlayerEntity targetPlayer;
            if (player != null) {
                targetPlayer = player;
            } else {
                targetPlayer = src.getPlayerOrException();
            }
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultInstance(), StickType.MARKER, targetPlayer.level.dimension());
            targetPlayer.addItem(markerStick);
            sendCmdFeedback(src, new TranslationTextComponent("Added RegionMarker to inventory of player '" + targetPlayer.getScoreboardName() + "'"));
            sendMessage(targetPlayer, new TranslationTextComponent("RegionMarker added to your inventory."));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, new TranslationTextComponent(RED + "Command needs a player as command source" + RESET));
            return 1;
        }
        return 0;
    }
}
