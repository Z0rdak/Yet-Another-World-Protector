package de.z0rdak.yawp.mixin;

import com.mojang.brigadier.ParseResults;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(Commands.class)
public abstract class CommandManagerMixin {

    @Inject(method = "performCommand", at = @At(value = "HEAD"), cancellable = true)
    public void execute(ParseResults<CommandSourceStack> parseResults, String command, CallbackInfo ci) {
        // check exec-command flag
        CommandSourceStack cmdSource = parseResults.getContext().getSource();
        if (cmdSource.isPlayer()) {
            ServerPlayer player = cmdSource.getPlayer();
            if (player != null) {
                FlagCheckRequest checkEvent = new FlagCheckRequest(player.blockPosition(), FlagRegister.PLAYER_USE_COMMANDS, getDimKey(player), player);
                if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, deny -> {
                    sendFlagMsg(deny);
                    ci.cancel();
                });
            }
        }
    }
}