package de.z0rdak.yawp.commands;

import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Objects;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.getPlayerArgument;
import static de.z0rdak.yawp.util.CommandUtil.literal;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.text.TextFormatting.RED;

public final class MarkerCommands {

    public static final LiteralArgumentBuilder<CommandSource> MARKER_COMMAND = register();

    private MarkerCommands() {
    }

    private static LiteralArgumentBuilder<CommandSource> register() {
        return literal(FLAG)
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
                        .executes(ctx -> resetStick(ctx.getSource())));
        /*
        CREATE REGION
         */
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
                    sendCmdFeedback(src, new TranslationTextComponent("Reset marker successfully.'"));
                    return 0;
                } else {
                    sendCmdFeedback(src, new TranslationTextComponent(RED + "Hold stick type is not a marker!" + RESET));
                    return 1;
                }
            } else {
                sendCmdFeedback(src, new TranslationTextComponent(RED + "Hold stick type is not a marker!" + RESET));
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

    public static int giveMarkerStick(CommandSource src, PlayerEntity player) {
        try {
            PlayerEntity targetPlayer;
            if (player != null) {
                targetPlayer = player;
            } else {
                targetPlayer = src.getPlayerOrException();
            }
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultInstance(), StickType.MARKER, targetPlayer.getCommandSenderWorld().dimension());
            targetPlayer.addItem(markerStick);
            sendCmdFeedback(src, new TranslationTextComponent("Added marker stick to inventory of player '" + player.getScoreboardName() + "'"));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, new TranslationTextComponent(RED + "Command needs a player as command source" + RESET));
            return 1;
        }
        return 0;
    }
}
