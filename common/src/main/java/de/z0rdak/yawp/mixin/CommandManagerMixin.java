package de.z0rdak.yawp.mixin;

import com.mojang.brigadier.ParseResults;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.CommandInterceptor;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.core.flag.RegionFlag.EXECUTE_COMMAND;
import static de.z0rdak.yawp.handler.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.api.FlagEvaluator.processCheck;
import static de.z0rdak.yawp.api.MessageSender.sendFlagMsg;

@Mixin(Commands.class)
public abstract class CommandManagerMixin {

    @Inject(method = "performCommand", at = @At(value = "HEAD"), cancellable = true)
    public void execute(ParseResults<CommandSourceStack> parseResults, String command, CallbackInfo ci) {
        // check mod command permissions
        int result = CommandInterceptor.handleModCommands(parseResults, command);
        if (result != 0) {
            ci.cancel();
        }
        // check exec-command flag
        CommandSourceStack cmdSource = parseResults.getContext().getSource();
        if (cmdSource.isPlayer()) {
            ServerPlayer player = cmdSource.getPlayer();
            if (player != null) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.blockPosition(), EXECUTE_COMMAND, getDimKey(player), player);
                if (Services.EVENT.post(checkEvent)) {
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